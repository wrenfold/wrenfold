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
template <typename T>
struct ConvertOutputArgType;
}  // namespace detail

struct NumericFunctionEvaluator {
  Expr operator()(const FunctionArgument& arg, const Expr&) const {
    auto it = values.find(arg);
    ASSERT(it != values.end(), "Missing function argument: ({}, {})", arg.ArgIndex(),
           arg.ElementIndex());
    return it->second;
  }

  template <typename T>
  std::enable_if_t<!std::is_same_v<T, FunctionArgument>, Expr> operator()(const T& input_typed,
                                                                          const Expr& input) const {
    if constexpr (T::IsLeafNode) {
      return input;
    } else {
      return MapChildren(input_typed, [this](const Expr& expr) {
        return Visit(expr, [this, &expr](const auto& x) { return this->operator()(x, expr); });
      });
    }
  }

  std::unordered_map<FunctionArgument, Expr, Hash<FunctionArgument>> values;
};

template <typename T>
struct ApplyNumericEvaluatorImpl;

template <typename T>
auto ApplyNumericEvaluator(const NumericFunctionEvaluator& evaluator, const T& input) {
  return ApplyNumericEvaluatorImpl<T>{}(evaluator, input);
}

template <>
struct ApplyNumericEvaluatorImpl<Expr> {
  double operator()(const NumericFunctionEvaluator& evaluator, const Expr& input) const {
    const Expr subs =
        Visit(input, [&evaluator, &input](const auto& x) { return evaluator(x, input); });
    const Float* f = CastPtr<Float>(subs);
    if (!f) {
      throw TypeError("Expression should be a floating point value. Got type {}: {}",
                      input.TypeName(), input.ToString());
    }
    return f->GetValue();
  }
};

template <>
struct ApplyNumericEvaluatorImpl<MatrixExpr> {
  Eigen::MatrixXd operator()(const NumericFunctionEvaluator& evaluator,
                             const MatrixExpr& input) const {
    Eigen::MatrixXd output{input.NumRows(), input.NumCols()};
    for (index_t i = 0; i < input.NumRows(); ++i) {
      for (index_t j = 0; j < input.NumCols(); ++j) {
        output(i, j) = ApplyNumericEvaluator(evaluator, input(i, j));
      }
    }
    return output;
  }
};

template <index_t Rows, index_t Cols>
struct ApplyNumericEvaluatorImpl<ta::StaticMatrix<Rows, Cols>> {
  Eigen::Matrix<double, Rows, Cols> operator()(const NumericFunctionEvaluator& evaluator,
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

template <typename ArgList, typename OutputTuple, std::size_t... InputIndices,
          std::size_t... OutputIndices>
auto CreateEvaluatorWithOutputExpressions(OutputTuple&& output_expressions,
                                          std::index_sequence<InputIndices...> input_seq,
                                          std::index_sequence<OutputIndices...> output_arg_seq) {
  return [output_expressions = std::move(output_expressions), input_seq, output_arg_seq](
             // Convert input arguments to const references:
             const typename detail::ConvertArgType<
                 typename TypeListElement<InputIndices, ArgList>::Type>::Type&... args,
             // Convert output expression types to non-const references:
             typename detail::ConvertOutputArgType<
                 std::tuple_element_t<OutputIndices, OutputTuple>>::Type&... output_args) {
    // Copy all the input arguments into the evaluator:
    NumericFunctionEvaluator evaluator{};
    CollectFunctionInputs(evaluator, input_seq, args...);

    // Create a tuple of non-const references to output arguments of this lambda, and write to it
    // by doing substitution with the NumericFunctionEvaluator:
    std::forward_as_tuple(output_args...) = std::apply(
        [&evaluator](const auto&... output_expression) {
          // Call .Value() to convert from OutputArg<> to the underlying expression.
          return std::make_tuple(ApplyNumericEvaluator(evaluator, output_expression.Value())...);
        },
        SelectFromTuple(output_expressions, output_arg_seq));

    // Now we need to deal w/ the return value:
    constexpr std::ptrdiff_t return_value_index = ReturnValueIndex<OutputTuple>::value;
    if constexpr (return_value_index >= 0) {
      // This function also has a return value that we need to fill out:
      return ApplyNumericEvaluator(evaluator,
                                   std::get<return_value_index>(output_expressions).Value());
    }
  };
}

// Given a function pointer to a symbolic function, create a lambda that accepts numeric types
// like double and Eigen::Matrix<double, ...>. The lambda converts the numeric arguments to the
// equivalent `Expr` type, and invokes the symbolic function. The results are converted back to
// numeric types.
template <typename ReturnType, typename... Args>
auto CreateEvaluator(ReturnType (*func)(Args... args)) {
  using ArgList = TypeList<std::decay_t<Args>...>;

  // Evaluate the function symbolically.
  // We don't substitute numerical values directly, because the function may wish to do symbolic
  // operations internally (like diff, subs, etc.). Instead, build symbolic expressions for every
  // output, and then substitute into those.
  std::tuple output_expressions =
      detail::InvokeWithOutputCapture<ArgList>(func, std::make_index_sequence<sizeof...(Args)>());

  constexpr auto output_indices = detail::SelectOutputArgIndices(output_expressions);
  return CreateEvaluatorWithOutputExpressions<ArgList>(
      std::move(output_expressions), std::make_index_sequence<sizeof...(Args)>(), output_indices);
}

namespace detail {
template <>
struct ConvertArgType<Expr> {
  using Type = double;
};

template <index_t Rows, index_t Cols>
struct ConvertArgType<ta::StaticMatrix<Rows, Cols>> {
  using Type = Eigen::Matrix<double, Rows, Cols>;
};

template <typename T>
struct ConvertOutputArgType<OutputArg<T>> {
  using Type = typename ConvertOutputArgType<T>::Type;
};

template <>
struct ConvertOutputArgType<Expr> {
  using Type = double;
};

template <index_t Rows, index_t Cols>
struct ConvertOutputArgType<ta::StaticMatrix<Rows, Cols>> {
  using Type = Eigen::Matrix<double, Rows, Cols>;
};

}  // namespace detail
}  // namespace math
