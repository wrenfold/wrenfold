// Copyright 2023 Gareth Cross
#pragma once
#include <Eigen/Core>

#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "function_evaluator_detail.h"
#include "type_annotations.h"
#include "visitor_impl.h"

// Utilities for evaluating symbolic functions into numeric values (like double,
// or Eigen::Matrix). This is for unit testing code-generated methods against
// numeric evaluation of the symbolic graph.
namespace math {
namespace ta = type_annotations;

namespace detail {

template <typename T>
struct ConvertArgType;

template <typename T>
struct InputExpressionFromNumeric;

template <typename T>
struct OutputExpressionFromNumeric;

// Convert a numeric argument type to the equivalent expression type.
// If the input type is a non-const reference, we interpret this as an output argument from
// a function and resolve to `OutputExpressionFromNumeric`. Otherwise, it is an input, and we
// use InputExpressionFromNumeric.
template <typename T>
struct ExpressionFromNumeric
    : public std::conditional_t<IsOutputArgument<T>,
                                OutputExpressionFromNumeric<std::remove_reference_t<T>>,
                                InputExpressionFromNumeric<std::decay_t<T>>> {};

// Convert expression argument type to numeric argument.
template <typename>
struct NumericFromExpression;

}  // namespace detail

// Given a function pointer to a symbolic function, create a lambda that accepts numeric types like
// double and Eigen::Matrix<double, ...>. The lambda converts the numeric arguments to the
// equivalent `Expr` type, and invokes the symbolic function. The results are converted back to
// numeric types.
template <typename ReturnType, typename... Args>
auto CreateEvaluator(ReturnType (*func)(Args... args)) {
  return [func](typename detail::ConvertArgType<Args>::Type&... args) {
    auto result = std::invoke(
        func,
        detail::ExpressionFromNumeric<typename detail::ConvertArgType<Args>::Type>{}(args)...);
    return detail::NumericFromExpression<decltype(result)>{}(result);
  };
}

namespace detail {
template <>
struct ConvertArgType<Expr> {
  using Type = const double;
};

template <>
struct ConvertArgType<Expr&> {
  using Type = double&;
};

template <index_t Rows, index_t Cols>
struct ConvertArgType<ta::StaticMatrix<Rows, Cols>> {
  using Type = const Eigen::Matrix<double, Rows, Cols>;
};

template <index_t Rows, index_t Cols>
struct ConvertArgType<ta::StaticMatrix<Rows, Cols>&> {
  using Type = Eigen::Matrix<double, Rows, Cols>&;
};

template <>
struct InputExpressionFromNumeric<double> {
  auto operator()(double x) const { return Expr{x}; }
};

template <index_t Rows, index_t Cols>
struct InputExpressionFromNumeric<Eigen::Matrix<double, Rows, Cols>> {
  // Convert eigen matrix to symbolic matrix of doubles.
  auto operator()(const Eigen::Matrix<double, Rows, Cols>& m) const {
    std::vector<Expr> args{};
    args.reserve(static_cast<std::size_t>(m.size()));
    for (std::size_t rows = 0; rows < Rows; ++rows) {
      for (std::size_t cols = 0; cols < Cols; ++cols) {
        args.emplace_back(m(rows, cols));
      }
    }
    return MatrixExpr::Create(Rows, Cols, std::move(args));
  }
};

template <index_t Rows, index_t Cols>
void CopyToNumericOutput(const ta::StaticMatrix<Rows, Cols>& function_output,
                         Eigen::Matrix<double, Rows, Cols>& destination) {
  for (std::size_t rows = 0; rows < Rows; ++rows) {
    for (std::size_t cols = 0; cols < Cols; ++cols) {
      const Float* const v = TryCast<Float>(function_output(rows, cols));
      ASSERT(v != nullptr, "Cannot convert output ({}, {}) to double. Expression has value: {}",
             rows, cols, function_output(rows, cols).ToString());
      destination(rows, cols) = v->GetValue();
    }
  }
}

inline void CopyToNumericOutput(const Expr& function_output, double& destination) {
  const Float* const v = TryCast<Float>(function_output);
  ASSERT(v != nullptr, "Cannot convert expression to double. Expression has value: {}",
         function_output.ToString());
  destination = v->GetValue();
}

template <>
struct NumericFromExpression<Expr> {
  double operator()(const Expr& x) const {
    double val;
    CopyToNumericOutput(x, val);
    return val;
  }
};

template <index_t Rows, index_t Cols>
struct NumericFromExpression<ta::StaticMatrix<Rows, Cols>> {
  Eigen::Matrix<double, Rows, Cols> operator()(const ta::StaticMatrix<Rows, Cols>& mat) const {
    Eigen::Matrix<double, Rows, Cols> val;
    CopyToNumericOutput(mat, val);
    return val;
  }
};

template <typename... Args>
struct NumericFromExpression<std::tuple<Args...>> {
  template <std::size_t... Indices>
  auto operator()(const std::tuple<Args...>& tuple, std::index_sequence<Indices...>) const {
    return std::make_tuple(NumericFromExpression<Args>{}(std::get<Indices>(tuple))...);
  }

  auto operator()(const std::tuple<Args...>& tuple) const {
    return this->operator()(tuple, std::make_index_sequence<sizeof...(Args)>());
  }
};

// Object that can be implicitly cast to `SourceType`. This is passed as an input argument
// to a function which writes to an `Expr` output argument. On destruction, it copies the
// values into a numeric matrix (Eigen).
template <typename SourceType, typename DestType>
struct OutputCapture {
  explicit OutputCapture(DestType& dest)
      : function_output_(InitializeStorage<SourceType>{}()), destination_(dest) {}

  ~OutputCapture() {
    // An exception might have been thrown during evaluation, in which case we shouldn't
    // try copying to the output. If we throw again, terminate will get triggered.
    if (!std::uncaught_exception()) {
      destination_ = NumericFromExpression<SourceType>{}(function_output_);
      //      CopyToNumericOutput(function_output_, destination_);
    }
  }

  // Cast to non-const reference to `T`.
  operator SourceType&() { return function_output_; }

 private:
  SourceType function_output_;
  DestType& destination_;
};

template <>
struct OutputExpressionFromNumeric<double> {
  auto operator()(double& x) const { return OutputCapture<Expr, double>(x); }
};

template <index_t Rows, index_t Cols>
struct OutputExpressionFromNumeric<Eigen::Matrix<double, Rows, Cols>> {
  auto operator()(Eigen::Matrix<double, Rows, Cols>& dest) {
    // Return object which will be cast automatically to `ta::StaticMatrix<Rows, Cols>`.
    return OutputCapture<ta::StaticMatrix<Rows, Cols>, Eigen::Matrix<double, Rows, Cols>>(dest);
  }
};

}  // namespace detail
}  // namespace math
