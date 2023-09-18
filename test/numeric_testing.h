// Copyright 2023 Gareth Cross
#pragma once
#include <Eigen/Core>

#include "expression.h"
#include "expressions/all_expressions.h"  //  Needed for NumericFunctionEvaluator
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
}  // namespace detail

struct NumericFunctionEvaluator {
  using ReturnType = Expr;

  Expr operator()(const FunctionArgument& arg, const Expr&) const {
    auto it = values.find(arg);
    ASSERT(it != values.end(), "Missing function argument: ({}, {})", arg.ArgIndex(),
           arg.ElementIndex());
    return it->second;
  }

  template <typename T>
  std::enable_if_t<!std::is_same_v<T, FunctionArgument>, Expr> operator()(const T& input_typed,
                                                                          const Expr& input) {
    if constexpr (T::IsLeafNode) {
      return input;
    } else {
      return MapChildren(input_typed,
                         [this](const Expr& expr) { return Visit(expr, *this, expr); });
    }
  }

  struct FunctionArgHasher {
    std::size_t operator()(const FunctionArgument& arg) const {
      return HashCombine(arg.ArgIndex(), arg.ElementIndex());
    }
  };

  std::unordered_map<FunctionArgument, Expr, FunctionArgHasher> values;
};

template <typename T>
struct ApplyNumericEvaluatorImpl;

template <typename T>
auto ApplyNumericEvaluator(NumericFunctionEvaluator& evaluator, const T& input) {
  return ApplyNumericEvaluatorImpl<T>{}(evaluator, input);
}

template <>
struct ApplyNumericEvaluatorImpl<Expr> {
  // TODO: Numeric evaluator should be const here, but must be non-const to satisfy Visit.
  double operator()(NumericFunctionEvaluator& evaluator, const Expr& input) const {
    const Expr subs = Visit(input, evaluator, input);
    const Float* f = CastPtr<Float>(subs);
    if (!f) {
      throw TypeError("Expression should be a floating point value. Got type {}: {}",
                      input.TypeName(), input.ToString());
    }
    return f->GetValue();
  }
};

template <index_t Rows, index_t Cols>
struct ApplyNumericEvaluatorImpl<ta::StaticMatrix<Rows, Cols>> {
  Eigen::Matrix<double, Rows, Cols> operator()(NumericFunctionEvaluator& evaluator,
                                               const MatrixExpr& input) const {
    ASSERT_EQUAL(input.NumRows(), Rows);
    ASSERT_EQUAL(input.NumCols(), Cols);
    Eigen::Matrix<double, Rows, Cols> output;
    for (index_t i = 0; i < Rows; ++i) {
      for (index_t j = 0; j < Cols; ++j) {
        // Apply the evaluator for `Expr`
        output(i, j) = ApplyNumericEvaluator(evaluator, input(i, j));
      }
    }
    return output;
  }
};

template <typename... Ts>
struct ApplyNumericEvaluatorImpl<std::tuple<Ts...>> {
  auto operator()(NumericFunctionEvaluator& evaluator, const std::tuple<Ts...>& tup) const {
    // std::apply is invalid for empty tuples:
    if constexpr (sizeof...(Ts) > 1) {
      return std::apply(
          [&evaluator](auto&&... element) {
            return std::make_tuple(ApplyNumericEvaluator(evaluator, element)...);
          },
          tup);
    } else if constexpr (sizeof...(Ts) == 1) {
      return std::make_tuple(ApplyNumericEvaluator(evaluator, std::get<0>(tup)));
    } else {
#ifdef _MSC_VER
      // We need this to silence MSVC
      (void)sizeof(evaluator);
      (void)sizeof(tup);
#endif
      return std::tuple<>{};
    }
  }
};

template <std::size_t Index, typename NumericArgType>
struct CollectFunctionInputImpl;

template <std::size_t Index>
struct CollectFunctionInputImpl<Index, double> {
  void operator()(NumericFunctionEvaluator& output, const double arg) const {
    output.values.emplace(FunctionArgument(Index, 0), Float::Create(arg));
  }
};

template <std::size_t Index, int Rows, int Cols>
struct CollectFunctionInputImpl<Index, Eigen::Matrix<double, Rows, Cols>> {
  void operator()(NumericFunctionEvaluator& output,
                  const Eigen::Matrix<double, Rows, Cols>& arg) const {
    static_assert(Rows > 0);
    static_assert(Cols > 0);
    for (int i = 0; i < Rows; ++i) {
      for (int j = 0; j < Cols; ++j) {
        const std::size_t element = static_cast<std::size_t>(i * Cols + j);
        output.values.emplace(FunctionArgument(Index, element), Float::Create(arg(i, j)));
      }
    }
  }
};

template <std::size_t Index, typename NumericArgType>
void CollectFunctionInput(NumericFunctionEvaluator& evaluator, const NumericArgType& numeric_arg) {
  CollectFunctionInputImpl<Index, NumericArgType>{}(evaluator, numeric_arg);
}

template <std::size_t... Indices, typename... NumericArgs>
void CollectFunctionInputs(NumericFunctionEvaluator& evaluator, std::index_sequence<Indices...>,
                           NumericArgs&&... args) {
  static_assert(sizeof...(Indices) <= sizeof...(NumericArgs));
#ifndef __GNUG__
  ([]() constexpr { static_assert(Indices <= sizeof...(NumericArgs)); }(), ...);
#endif
  // Access args specified by `Indices` and call `CollectFunctionInput` on all of them.
  (CollectFunctionInput<Indices>(evaluator, std::get<Indices>(std::forward_as_tuple(args...))),
   ...);
}

// Given a function pointer to a symbolic function, create a lambda that accepts numeric types
// like double and Eigen::Matrix<double, ...>. The lambda converts the numeric arguments to the
// equivalent `Expr` type, and invokes the symbolic function. The results are converted back to
// numeric types.
template <typename ReturnType, typename... Args>
auto CreateEvaluator(ReturnType (*func)(Args... args)) {
  using ArgList = TypeList<Args...>;

  // Evaluate the function symbolically.
  // We don't substitute numerical values directly, because the function may wish to do symbolic
  // operations internally (like diff, subs, etc.). Instead, build symbolic expressions for every
  // output, and then substitute into those.
  auto [result_expression, output_arg_expressions] =
      detail::InvokeWithOutputCapture<ArgList>(func, std::make_index_sequence<sizeof...(Args)>());

  // Get indices of input and output arguments:
  constexpr auto input_indices = detail::FilterArguments<true, Args...>();
  constexpr auto output_indices = detail::FilterArguments<false, Args...>();

  return [return_values = std::move(result_expression),
          output_args = std::move(output_arg_expressions), input_indices,
          output_indices](typename detail::ConvertArgType<Args>::Type&... args) {
    // Copy all the input arguments into the evaluator:
    NumericFunctionEvaluator evaluator{};
    CollectFunctionInputs(evaluator, input_indices, args...);

    // Create a tuple of non-const references to output arguments of this lambda, and write to it
    // by doing substitution with the NumericFunctionEvaluator:
    SelectFromTuple(std::forward_as_tuple(args...), output_indices) =
        ApplyNumericEvaluator(evaluator, output_args);

    if constexpr (!std::is_same_v<ReturnType, void>) {
      return ApplyNumericEvaluator(evaluator, return_values);
    }
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

}  // namespace detail
}  // namespace math
