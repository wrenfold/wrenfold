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
struct convert_arg_type;
template <typename T>
struct convert_output_arg_type;
}  // namespace detail

// The numeric function evaluator operates in two steps:
// 1. We traverse the input arguments and copy their values into `values` as `Float` objects.
// 2. We traverse an expression tree and replace `FunctionArgument` objects by their corresponding
// `Float`.
struct NumericFunctionEvaluator {
  Expr operator()(const FunctionArgument& arg, const Expr&) const {
    auto it = values.find(arg);
    ASSERT(it != values.end(), "Missing function argument: ({}, {})", arg.arg_index(),
           arg.element_index());
    return it->second;
  }

  template <typename T>
  std::enable_if_t<!std::is_same_v<T, FunctionArgument>, Expr> operator()(const T& input_typed,
                                                                          const Expr& input) const {
    if constexpr (T::IsLeafNode) {
      return input;
    } else {
      return input_typed.map_children([this](const Expr& expr) {
        return Visit(expr, [this, &expr](const auto& x) { return this->operator()(x, expr); });
      });
    }
  }

  std::unordered_map<FunctionArgument, Expr, hash_struct<FunctionArgument>> values;
};

template <typename T>
struct apply_numeric_evaluator_impl;

template <typename T>
auto apply_numeric_evaluator(const NumericFunctionEvaluator& evaluator, const T& input) {
  return apply_numeric_evaluator_impl<T>{}(evaluator, input);
}

template <>
struct apply_numeric_evaluator_impl<Expr> {
  double operator()(const NumericFunctionEvaluator& evaluator, const Expr& input) const {
    const Expr subs =
        Visit(input, [&evaluator, &input](const auto& x) { return evaluator(x, input); });
    if (const Float* f = cast_ptr<Float>(subs); f != nullptr) {
      return f->get_value();
    } else if (const Integer* i = cast_ptr<Integer>(subs); i != nullptr) {
      // TODO: Support returning the correct type here.
      return static_cast<double>(i->get_value());
    } else {
      throw TypeError("Expression should be a floating point value or integer. Got type {}: {}",
                      input.type_name(), input.to_string());
    }
  }
};

template <>
struct apply_numeric_evaluator_impl<MatrixExpr> {
  Eigen::MatrixXd operator()(const NumericFunctionEvaluator& evaluator,
                             const MatrixExpr& input) const {
    Eigen::MatrixXd output{input.rows(), input.cols()};
    for (index_t i = 0; i < input.rows(); ++i) {
      for (index_t j = 0; j < input.cols(); ++j) {
        output(i, j) = apply_numeric_evaluator(evaluator, input(i, j));
      }
    }
    return output;
  }
};

template <index_t Rows, index_t Cols>
struct apply_numeric_evaluator_impl<ta::StaticMatrix<Rows, Cols>> {
  Eigen::Matrix<double, Rows, Cols> operator()(const NumericFunctionEvaluator& evaluator,
                                               const MatrixExpr& input) const {
    ASSERT_EQUAL(input.rows(), Rows);
    ASSERT_EQUAL(input.cols(), Cols);
    Eigen::Matrix<double, Rows, Cols> output;
    for (index_t i = 0; i < Rows; ++i) {
      for (index_t j = 0; j < Cols; ++j) {
        // Apply the evaluator for `Expr`
        output(i, j) = apply_numeric_evaluator(evaluator, input(i, j));
      }
    }
    return output;
  }
};

template <std::size_t Index, typename NumericArgType>
struct collect_function_input_impl;

template <std::size_t Index>
struct collect_function_input_impl<Index, double> {
  void operator()(NumericFunctionEvaluator& output, const double arg) const {
    output.values.emplace(FunctionArgument(Index, 0), Float::create(arg));
  }
};

template <std::size_t Index, int Rows, int Cols>
struct collect_function_input_impl<Index, Eigen::Matrix<double, Rows, Cols>> {
  void operator()(NumericFunctionEvaluator& output,
                  const Eigen::Matrix<double, Rows, Cols>& arg) const {
    static_assert(Rows > 0);
    static_assert(Cols > 0);
    for (int i = 0; i < Rows; ++i) {
      for (int j = 0; j < Cols; ++j) {
        const std::size_t element = static_cast<std::size_t>(i * Cols + j);
        output.values.emplace(FunctionArgument(Index, element), Float::create(arg(i, j)));
      }
    }
  }
};

template <std::size_t Index, typename NumericArgType>
void collect_function_input(NumericFunctionEvaluator& evaluator,
                            const NumericArgType& numeric_arg) {
  collect_function_input_impl<Index, NumericArgType>{}(evaluator, numeric_arg);
}

template <std::size_t... Indices, typename... NumericArgs>
void collect_function_inputs(NumericFunctionEvaluator& evaluator, std::index_sequence<Indices...>,
                             NumericArgs&&... args) {
  static_assert(sizeof...(Indices) <= sizeof...(NumericArgs));
#ifndef __GNUG__
  ([]() constexpr { static_assert(Indices <= sizeof...(NumericArgs)); }(), ...);
#endif
  // Access args specified by `Indices` and call `collect_function_input` on all of them.
  (collect_function_input<Indices>(evaluator, std::get<Indices>(std::forward_as_tuple(args...))),
   ...);
}

template <typename ArgList, typename OutputTuple, std::size_t... InputIndices,
          std::size_t... OutputIndices>
auto create_evaluator_with_output_expressions(
    OutputTuple&& output_expressions, std::index_sequence<InputIndices...> input_seq,
    std::index_sequence<OutputIndices...> output_arg_seq) {
  return [output_expressions = std::move(output_expressions), input_seq, output_arg_seq](
             // Convert input arguments to const references:
             const detail::convert_arg_type_t<type_list_element_t<InputIndices, ArgList>>&... args,
             // Convert output expression types to non-const references:
             detail::convert_output_arg_type_t<
                 std::tuple_element_t<OutputIndices, OutputTuple>>&... output_args) {
    // Copy all the input arguments into the evaluator:
    NumericFunctionEvaluator evaluator{};
    collect_function_inputs(evaluator, input_seq, args...);

    // Create a tuple of non-const references to output arguments of this lambda, and write to it
    // by doing substitution with the NumericFunctionEvaluator:
    std::forward_as_tuple(output_args...) = std::apply(
        [&evaluator](const auto&... output_expression) {
          // Call .value() to convert from OutputArg<> to the underlying expression.
          return std::make_tuple(apply_numeric_evaluator(evaluator, output_expression.value())...);
        },
        select_from_tuple(output_expressions, output_arg_seq));

    // Now we need to deal w/ the return value:
    constexpr std::ptrdiff_t return_val_index = return_value_index<OutputTuple>::value;
    if constexpr (return_val_index >= 0) {
      // This function also has a return value that we need to fill out:
      return apply_numeric_evaluator(evaluator,
                                     std::get<return_val_index>(output_expressions).value());
    }
  };
}

// Given a function pointer to a symbolic function, create a lambda that accepts numeric types
// like double and Eigen::Matrix<double, ...>. The lambda converts the numeric arguments to the
// equivalent `Expr` type, and invokes the symbolic function. The results are converted back to
// numeric types.
template <typename ReturnType, typename... Args>
auto create_evaluator(ReturnType (*func)(Args... args)) {
  using ArgList = type_list<std::decay_t<Args>...>;

  // Evaluate the function symbolically.
  // We don't substitute numerical values directly, because the function may wish to do symbolic
  // operations internally (like diff, subs, etc.). Instead, build symbolic expressions for every
  // output, and then substitute into those.
  std::tuple output_expressions = detail::invoke_with_output_capture<ArgList>(
      func, std::make_index_sequence<sizeof...(Args)>());

  constexpr auto output_indices = detail::select_output_arg_indices(output_expressions);
  return create_evaluator_with_output_expressions<ArgList>(
      std::move(output_expressions), std::make_index_sequence<sizeof...(Args)>(), output_indices);
}

namespace detail {
template <>
struct convert_arg_type<Expr> {
  using type = double;
};

template <index_t Rows, index_t Cols>
struct convert_arg_type<ta::StaticMatrix<Rows, Cols>> {
  using type = Eigen::Matrix<double, Rows, Cols>;
};
template <typename T>
using convert_arg_type_t = typename convert_arg_type<T>::type;

template <typename T>
struct convert_output_arg_type<OutputArg<T>> {
  using type = typename convert_output_arg_type<T>::type;
};
template <>
struct convert_output_arg_type<Expr> {
  using type = double;
};
template <index_t Rows, index_t Cols>
struct convert_output_arg_type<ta::StaticMatrix<Rows, Cols>> {
  using type = Eigen::Matrix<double, Rows, Cols>;
};
template <typename T>
using convert_output_arg_type_t = typename convert_output_arg_type<T>::type;

}  // namespace detail
}  // namespace math
