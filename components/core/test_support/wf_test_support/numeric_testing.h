// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <Eigen/Core>

#include "wf/code_generation/function_evaluator.h"
#include "wf/evaluate.h"
#include "wf/expression.h"
#include "wf/expressions/compound_expression_element.h"
#include "wf/expressions/variable.h"
#include "wf/substitute.h"
#include "wf/type_annotations.h"

// Utilities for evaluating symbolic functions into numeric values (like double, or Eigen::Matrix).
// This is for unit testing code-generated methods against numeric evaluation of the symbolic graph.
namespace wf {
namespace ta = type_annotations;

namespace detail {
template <typename T, typename = void>
struct convert_arg_type;
template <typename T, typename = void>
struct convert_output_arg_type;
template <typename T>
using convert_arg_type_t = typename convert_arg_type<T>::type;
template <typename T>
using convert_output_arg_type_t = typename convert_output_arg_type<T>::type;
}  // namespace detail

// Trait we implement to allow conversion of symbolic custom types to native ones.
template <typename T, typename = void>
struct custom_type_native_converter;

// Enable if numeric type can be converted to symbolic type `T`.
// This is to switch the conversion `custom_type_native_converter<T>::native_type` --> `T`.
// std::invoke_result_t is broken here on gcc 11, so we use decltype() instead.
template <typename T, typename Type = void>
using enable_if_implements_symbolic_from_native_conversion_t = std::enable_if_t<
    std::is_same_v<
        decltype(std::declval<custom_type_native_converter<T>>()(
            std::declval<const typename custom_type_native_converter<T>::native_type&>())),

        T>,
    Type>;

// Enable if symbolic type `T` can be converted to a numeric type.
// This is to switch the conversion `T` --> `custom_type_native_converter<T>::native_type`.
template <typename T, typename Type = void>
using enable_if_implements_native_from_symbolic_conversion_t =
    std::enable_if_t<std::is_same_v<decltype(std::declval<custom_type_native_converter<T>>()(
                                        std::declval<const T&>())),
                                    typename custom_type_native_converter<T>::native_type>,
                     Type>;

// The numeric function evaluator operates in two steps:
// 1. Replace `variable` expression with float values.
// 2. Collapse all other numeric values into floats.
struct numeric_function_evaluator {
  substitute_variables_visitor substitute{};
  evaluate_visitor evaluate{};
};

// We implement this trait
template <typename T, typename = void>
struct compute_function_output_struct;

template <typename T, typename TypeAlias>
auto compute_function_output(numeric_function_evaluator& evaluator, const T& input,
                             const TypeAlias& type) {
  static_assert(std::is_constructible_v<type_variant, TypeAlias>,
                "TypeAlias should be something we can convert to type_variant.");
  return compute_function_output_struct<T>{}(evaluator, input, type);
}

template <>
struct compute_function_output_struct<scalar_expr> {
  double operator()(numeric_function_evaluator& evaluator, const scalar_expr& input,
                    const scalar_type&) const {
    const scalar_expr subs = evaluator.substitute(input);
    const scalar_expr evaluated = evaluator.evaluate(subs);
    if (const float_constant* f = get_if<const float_constant>(evaluated); f != nullptr) {
      return f->value();
    } else {
      throw type_error("Expression should be a floating point value. Got type {}: {}",
                       subs.type_name(), subs);
    }
  }
};

template <index_t Rows, index_t Cols>
struct compute_function_output_struct<ta::static_matrix<Rows, Cols>> {
  Eigen::Matrix<double, Rows, Cols> operator()(numeric_function_evaluator& evaluator,
                                               const matrix_expr& input, const matrix_type&) const {
    WF_ASSERT_EQ(input.rows(), Rows);
    WF_ASSERT_EQ(input.cols(), Cols);
    Eigen::Matrix<double, Rows, Cols> output;
    for (index_t i = 0; i < Rows; ++i) {
      for (index_t j = 0; j < Cols; ++j) {
        // TODO: Get the scalar type from `matrix_type`.
        output(i, j) = compute_function_output(evaluator, input(i, j),
                                               scalar_type(numeric_primitive_type::floating_point));
      }
    }
    return output;
  }
};

// Support conversion of symbolic custom struct `T` back the native type.
template <typename T>
struct compute_function_output_struct<T,
                                      enable_if_implements_native_from_symbolic_conversion_t<T>> {
  using native_type = typename custom_type_native_converter<T>::native_type;

  native_type operator()(numeric_function_evaluator& evaluator, const T& input,
                         const annotated_custom_type<T>& type) const {
    // Get the symbolic outputs for this type.
    std::vector<scalar_expr> symbolic_outputs;
    type.copy_output_expressions(input, symbolic_outputs);
    // Turn them into float expressions:
    for (scalar_expr& expr : symbolic_outputs) {
      expr = evaluator.evaluate(evaluator.substitute(expr));
      if (!expr.is_type<float_constant>()) {
        throw type_error("Expression should be a floating point value. Got type `{}`: {}",
                         expr.type_name(), expr);
      }
    }
    // Put them back the symbolic type:
    auto [numeric_input, _] =
        type.initialize_from_expressions(absl::Span<const scalar_expr>{symbolic_outputs});
    // Call the user provided converter:
    return custom_type_native_converter<T>{}(numeric_input);
  }
};

template <typename SymbolicArgType, typename = void>
struct collect_function_input;

template <typename T>
using enable_if_floating_point_t = std::enable_if_t<std::is_floating_point_v<T>>;

// Convert floats/doubles to single variable expression.
template <>
struct collect_function_input<scalar_expr> {
  template <typename U, typename = enable_if_floating_point_t<U>>
  void operator()(substitute_variables_visitor& output, const std::size_t arg_index, const U arg,
                  const scalar_type& scalar) const {
    const auto a = static_cast<float_constant::value_type>(arg);
    const bool added = output.add_substitution(
        make_expr<function_argument_variable>(arg_index, 0, scalar.numeric_type()),
        make_expr<float_constant>(a));
    WF_ASSERT(added);
  }
};

// Convert eigen matrices to matrix_expr.
template <index_t Rows, index_t Cols>
struct collect_function_input<type_annotations::static_matrix<Rows, Cols>> {
  static_assert(Rows > 0);
  static_assert(Cols > 0);

  template <typename U, typename = enable_if_floating_point_t<U>>
  void operator()(substitute_variables_visitor& output, const std::size_t arg_index,
                  const Eigen::Matrix<U, Rows, Cols>& arg, const matrix_type&) const {
    for (int i = 0; i < Rows; ++i) {
      for (int j = 0; j < Cols; ++j) {
        const std::size_t element = static_cast<std::size_t>(i * Cols + j);
        const auto a_ij = static_cast<float_constant::value_type>(arg(i, j));
        const bool added =
            output.add_substitution(make_expr<function_argument_variable>(
                                        arg_index, element, numeric_primitive_type::floating_point),
                                    scalar_expr(a_ij));
        WF_ASSERT(added);
      }
    }
  }
};

// Convert custom type `T`.
template <typename T>
struct collect_function_input<T, enable_if_implements_symbolic_from_native_conversion_t<T>> {
  using native_type = typename custom_type_native_converter<T>::native_type;

  void operator()(substitute_variables_visitor& output, const std::size_t arg_index,
                  const native_type& arg, const annotated_custom_type<T>& type) const {
    static_assert(implements_custom_type_registrant_v<T>,
                  "Type must implement custom_type_registrant<T>");
    // Convert to the symbolic type (with numeric values), then extract the values into a flat
    // vector:
    const T symbolic_arg_with_numeric_values = custom_type_native_converter<T>{}(arg);

    std::vector<scalar_expr> numeric_expressions;
    type.copy_output_expressions(symbolic_arg_with_numeric_values, numeric_expressions);

    // Configure the substitutions:
    const compound_expr provenance = create_custom_type_argument(type.inner(), arg_index);
    for (std::size_t i = 0; i < numeric_expressions.size(); ++i) {
      const bool added = output.add_substitution(
          make_expr<compound_expression_element>(provenance, i), std::move(numeric_expressions[i]));
      WF_ASSERT(added);
    }
  }
};

// `ArgSymbolicTypes` are the custom user-defined types that contain symblic expressions.
// `ArgTypes` are type descriptors like scalar_type, custom_type, etc.
// `OutputTypes` are also type descriptors.
template <typename OutputTuple, typename... ArgSymbolicTypes, typename... ArgTypes,
          typename... OutputTypes, std::size_t... OutputArgIndices,
          std::size_t... ReturnValueIndices>
auto create_evaluator_with_output_expressions(type_list<ArgSymbolicTypes...>,
                                              std::tuple<ArgTypes...> input_arg_types,
                                              OutputTuple&& output_expressions,
                                              std::tuple<OutputTypes...> output_types,
                                              std::index_sequence<OutputArgIndices...>,
                                              std::index_sequence<ReturnValueIndices...>) {
  return [output_expressions = std::move(output_expressions),
          input_arg_types = std::move(input_arg_types), output_types = std::move(output_types)](
             // Convert input arguments to const references:
             const detail::convert_arg_type_t<ArgSymbolicTypes>&... args,
             // Convert output expression types to non-const references:
             detail::convert_output_arg_type_t<
                 std::tuple_element_t<OutputArgIndices, OutputTuple>>&... output_args) {
    // Copy all the input arguments into the evaluator:
    numeric_function_evaluator evaluator{};
    zip_enumerate_tuples(
        [&evaluator](auto index, const auto& numeric_arg, const auto& arg_type) {
          using symbolic_arg_type = type_list_element_t<index(), type_list<ArgSymbolicTypes...>>;
          collect_function_input<symbolic_arg_type>{}(evaluator.substitute, index(), numeric_arg,
                                                      arg_type);
        },
        std::make_tuple(std::cref(args)...), input_arg_types);

    std::tuple evaluated_outputs = zip_tuples(
        [&evaluator](const auto& output_value, const auto& type) {
          // Call .value() to convert from output_arg<> to the underlying expression.
          return compute_function_output(evaluator, output_value.value(), type);
        },
        output_expressions, output_types);

    std::tie(output_args...) =
        std::forward_as_tuple(std::get<OutputArgIndices>(std::move(evaluated_outputs))...);

    if constexpr (sizeof...(ReturnValueIndices) == 1) {
      return std::get<ReturnValueIndices...>(std::move(evaluated_outputs));
    } else if constexpr (sizeof...(ReturnValueIndices) > 1) {
      return std::make_tuple(std::get<ReturnValueIndices>(std::move(evaluated_outputs))...);
    }
  };
}

// Given a function pointer to a symbolic function, create a lambda that accepts numeric types
// like double and Eigen::Matrix<double, ...>. The lambda converts the numeric arguments to the
// equivalent `scalar_expr` type, and invokes the symbolic function. The results are converted back
// to numeric types.
template <typename ReturnType, typename... Args>
auto create_evaluator(ReturnType (*func)(Args... args)) {
  // Scrape the types of the input arguments:
  custom_type_registry registry{};
  std::tuple arg_types = detail::record_arg_types(registry, type_list<Args...>{});

  // Evaluate the function symbolically.
  // We don't substitute numerical values directly, because the function may wish to do symbolic
  // operations internally (like diff, subs, etc.). Instead, build symbolic expressions for every
  // output, and then substitute into those.
  std::tuple output_expressions = detail::invoke_with_symbolic_inputs(func, arg_types);

  // Scrape the types of the output expressions:
  using output_symbolic_types = type_list_from_tuple_t<decltype(output_expressions)>;
  std::tuple output_types = detail::record_arg_types(registry, output_symbolic_types{});

  // Determine the indices of output arguments in `output_expressions`.
  // This index_sequence will filter out any return values.
  constexpr auto output_arg_indices = filter_type_sequence<is_output_arg>(output_symbolic_types{});
  constexpr auto return_value_indices =
      filter_type_sequence<is_return_value>(output_symbolic_types{});

  return create_evaluator_with_output_expressions(
      type_list<std::decay_t<Args>...>{}, std::move(arg_types), std::move(output_expressions),
      std::move(output_types), output_arg_indices, return_value_indices);
}

namespace detail {
template <>
struct convert_arg_type<scalar_expr> {
  using type = double;
};

template <index_t Rows, index_t Cols>
struct convert_arg_type<ta::static_matrix<Rows, Cols>> {
  using type = Eigen::Matrix<double, Rows, Cols>;
};

template <typename T>
struct convert_arg_type<T, enable_if_implements_native_from_symbolic_conversion_t<T>> {
  using type = typename custom_type_native_converter<T>::native_type;
};

template <typename T>
struct convert_output_arg_type<output_arg<T>> {
  using type = typename convert_output_arg_type<T>::type;
};
template <>
struct convert_output_arg_type<scalar_expr> {
  using type = double;
};
template <index_t Rows, index_t Cols>
struct convert_output_arg_type<ta::static_matrix<Rows, Cols>> {
  using type = Eigen::Matrix<double, Rows, Cols>;
};
template <typename T>
struct convert_output_arg_type<T, enable_if_implements_native_from_symbolic_conversion_t<T>> {
  using type = typename custom_type_native_converter<T>::native_type;
};

}  // namespace detail
}  // namespace wf
