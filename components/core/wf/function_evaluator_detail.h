// Copyright 2023 Gareth Cross
#pragma once

#include "wf/code_generation/ast.h"
#include "wf/code_generation/expression_group.h"
#include "wf/constants.h"
#include "wf/expressions/variable.h"
#include "wf/output_annotations.h"
#include "wf/template_utils.h"
#include "wf/type_annotations.h"

namespace math {
namespace detail {

template <typename T>
struct copy_output_expressions;

template <>
struct copy_output_expressions<Expr> {
  void operator()(const Expr& val, std::vector<Expr>& outputs) const { outputs.push_back(val); }
};

template <>
struct copy_output_expressions<MatrixExpr> {
  void operator()(const MatrixExpr& val, std::vector<Expr>& outputs) const {
    outputs.reserve(val.size());
    for (index_t row = 0; row < val.rows(); ++row) {
      for (index_t col = 0; col < val.cols(); ++col) {
        outputs.push_back(val(row, col));
      }
    }
  }
};

template <index_t Rows, index_t Cols>
struct copy_output_expressions<type_annotations::static_matrix<Rows, Cols>> {
  void operator()(const type_annotations::static_matrix<Rows, Cols>& val,
                  std::vector<Expr>& outputs) const {
    copy_output_expressions<MatrixExpr>{}.operator()(val, outputs);
  }
};

template <typename T>
expression_group create_expression_group(const T& tuple_element) {
  std::vector<Expr> expressions;

  // TInner is the inner type of `return_value` or `output_arg`.
  using TInner = std::decay_t<decltype(tuple_element.value())>;
  copy_output_expressions<TInner>{}(tuple_element.value(), expressions);

  if constexpr (is_return_value<T>::value) {
    output_key key{expression_usage::return_value, ""};
    return expression_group(std::move(expressions), std::move(key));
  } else {
    const output_arg<TInner>& as_output_arg = tuple_element;
    output_key key{as_output_arg.is_optional() ? expression_usage::optional_output_argument
                                               : expression_usage::output_argument,
                   as_output_arg.name()};
    return expression_group(std::move(expressions), std::move(key));
  }
}

template <typename... Ts, std::size_t... Indices>
void copy_output_expression_from_tuple(const std::tuple<Ts...>& output_tuple,
                                       std::vector<expression_group>& groups,
                                       std::index_sequence<Indices...>) {
  static_assert(std::conjunction_v<is_output_arg_or_return_value<Ts>...>,
                "All returned elements of the tuple must be explicitly marked as `return_value` or "
                "`output_arg`.");
  static_assert(count_return_values_v<Ts...> <= 1, "Only one return value is allowed.");
  groups.reserve(sizeof...(Ts));
  // Comma operator ensures the order of evaluation here will be left -> right.
  (groups.push_back(create_expression_group(std::get<Indices>(output_tuple))), ...);
}

// Create an index sequence, so we can invoke `copy_output_expression_from_tuple` over all the
// elements of the tuple.
template <typename... Ts>
void copy_output_expression_from_tuple(const std::tuple<Ts...>& output_tuple,
                                       std::vector<expression_group>& groups) {
  copy_output_expression_from_tuple(output_tuple, groups,
                                    std::make_index_sequence<sizeof...(Ts)>());
}

template <typename T>
struct record_output;

template <typename T>
struct record_output<return_value<T>> {
  void operator()(ast::function_signature& desc, const return_value<T>& output) const {
    // This is a return value.
    if constexpr (std::is_same_v<Expr, T>) {
      desc.return_value = ast::scalar_type(code_numeric_type::floating_point);
    } else {
      const MatrixExpr& mat = output.value();
      desc.return_value = ast::matrix_type(mat.rows(), mat.cols());
    }
  }
};

template <typename T>
struct record_output<output_arg<T>> {
  void operator()(ast::function_signature& desc, const output_arg<T>& output) const {
    if constexpr (std::is_same_v<Expr, T>) {
      desc.add_argument(output.name(), ast::scalar_type(code_numeric_type::floating_point),
                        output.is_optional() ? ast::argument_direction::optional_output
                                             : ast::argument_direction::output);
    } else {
      // todo: static assert this is StaticMatrix
      desc.add_argument(output.name(),
                        ast::matrix_type(output.value().rows(), output.value().cols()),
                        output.is_optional() ? ast::argument_direction::optional_output
                                             : ast::argument_direction::output);
    }
  }
};

template <typename T>
struct record_input_argument;

template <>
struct record_input_argument<Expr> {
  void operator()(ast::function_signature& desc, const arg& arg) const {
    desc.add_argument(arg.name(), ast::scalar_type(code_numeric_type::floating_point),
                      ast::argument_direction::input);
  }
};

template <index_t Rows, index_t Cols>
struct record_input_argument<type_annotations::static_matrix<Rows, Cols>> {
  void operator()(ast::function_signature& desc, const arg& arg) const {
    desc.add_argument(arg.name(), ast::matrix_type(Rows, Cols), ast::argument_direction::input);
  }
};

template <typename ArgList, std::size_t... Indices, std::size_t N>
void record_input_args(ast::function_signature& desc, const std::array<arg, N>& args,
                       std::index_sequence<Indices...>) {
  (record_input_argument<std::decay_t<type_list_element_t<Indices, ArgList>>>{}(desc,
                                                                                args[Indices]),
   ...);
}

template <typename T>
struct construct_function_argument;

template <>
struct construct_function_argument<Expr> {
  auto operator()(std::size_t arg_index) const {
    return variable::create_function_argument(arg_index, 0);
  }
};

template <index_t Rows, index_t Cols>
struct construct_function_argument<type_annotations::static_matrix<Rows, Cols>> {
  auto operator()(std::size_t arg_index) const {
    std::vector<Expr> expressions{};
    expressions.reserve(static_cast<std::size_t>(Rows * Cols));
    for (std::size_t i = 0; i < Rows * Cols; ++i) {
      expressions.push_back(variable::create_function_argument(arg_index, i));
    }
    MatrixExpr expr = MatrixExpr::create(Rows, Cols, std::move(expressions));
    return type_annotations::static_matrix<Rows, Cols>(std::move(expr));
  }
};

template <std::size_t Index, typename ArgList>
auto construct_function_args() {
  using T = type_list_element_t<Index, ArgList>;
  using TDecay = std::decay_t<T>;
  // Increase input_index every time we hit an input argument.
  return construct_function_argument<TDecay>{}(Index);
}

template <std::size_t... A, std::size_t... B>
constexpr std::index_sequence<A..., B...> operator+(std::index_sequence<A...>,
                                                    std::index_sequence<B...>) {
  return {};
}

// Return an index_sequence<> that contains the indices of just `output_arg` objects.
// This is used to filter out any return values.
template <typename TupleType, std::size_t... Indices>
constexpr auto select_output_arg_indices(std::index_sequence<Indices...>) {
  return (std::conditional_t<is_output_arg<std::tuple_element_t<Indices, TupleType>>::value,
                             std::index_sequence<Indices>, std::index_sequence<>>() +
          ...);
}
template <typename... Ts>
constexpr auto select_output_arg_indices(const std::tuple<Ts...>&) {
  return select_output_arg_indices<std::tuple<Ts...>>(std::make_index_sequence<sizeof...(Ts)>());
}

// Invoke the provided callable and capture the output expressions. First builds a tuple of input
// arguments by constructing `function_argument_variable` expressions for every input arg of
// `callable`. The resulting expressions are returned as a tuple of `output_arg<>` or
// `return_value<>`.
template <typename ArgList, typename Callable, std::size_t... Indices>
auto invoke_with_output_capture(Callable&& callable, std::index_sequence<Indices...>) {
  static_assert(sizeof...(Indices) <= type_list_size_v<ArgList>);
  // Create a tuple of arguments w/ Variable expressions.
  auto args = std::make_tuple(construct_function_args<Indices, ArgList>()...);

  // Call the user provided function with the args we just created:
  using return_type =
      std::invoke_result_t<Callable, decltype(std::get<Indices>(std::move(args)))...>;
  static_assert(!std::is_same_v<return_type, void>, "Return type should not be void.");

  if constexpr (is_tuple_v<return_type>) {
    return std::invoke(std::forward<Callable>(callable), std::get<Indices>(std::move(args))...);
  } else {
    // Function returns a single expression, so convert to a tuple.
    auto return_expr =
        std::invoke(std::forward<Callable>(callable), std::get<Indices>(std::move(args))...);

    if constexpr (is_return_value<decltype(return_expr)>::value) {
      return std::make_tuple(std::move(return_expr));
    } else {
      return std::make_tuple(return_value(std::move(return_expr)));
    }
  }
}

}  // namespace detail
}  // namespace math
