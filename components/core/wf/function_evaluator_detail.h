// Copyright 2023 Gareth Cross
#pragma once

#include "wf/code_generation/ast.h"
#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/function_description.h"
#include "wf/expressions/variable.h"
#include "wf/output_annotations.h"
#include "wf/template_utils.h"
#include "wf/type_annotations.h"

namespace wf {
namespace detail {

template <typename T>
struct record_output;

template <typename T>
struct record_output<return_value<T>> {
  void operator()(function_description& desc, const return_value<T>& output) const {
    // This is a return value, which may be either a scalar or a matrix:
    if constexpr (std::is_same_v<Expr, T>) {
      desc.set_return_value(scalar_type{code_numeric_type::floating_point}, {output.value()});
    } else {
      const MatrixExpr& mat = output.value();
      desc.set_return_value(matrix_type{mat.rows(), mat.cols()}, mat.to_vector());
    }
  }
};

template <typename T>
struct record_output<output_arg<T>> {
  void operator()(function_description& desc, const output_arg<T>& output) const {
    if constexpr (std::is_same_v<Expr, T>) {
      desc.add_output_argument(output.name(), scalar_type{code_numeric_type::floating_point},
                               output.is_optional(), {output.value()});
    } else {
      const MatrixExpr& mat = output.value();
      desc.add_output_argument(output.name(), matrix_type{mat.rows(), mat.cols()},
                               output.is_optional(), mat.to_vector());
    }
  }
};

template <typename T>
struct record_input_argument;

template <>
struct record_input_argument<Expr> {
  void operator()(function_description& desc, const arg& arg) const {
    desc.add_input_argument(arg.name(), scalar_type(code_numeric_type::floating_point));
  }
};

template <index_t Rows, index_t Cols>
struct record_input_argument<type_annotations::static_matrix<Rows, Cols>> {
  void operator()(function_description& desc, const arg& arg) const {
    desc.add_input_argument(arg.name(), matrix_type(Rows, Cols));
  }
};

// Expand over `Indices` and record every inpu
template <typename ArgList, std::size_t... Indices>
void record_input_args(function_description& desc, const std::vector<arg>& args,
                       std::index_sequence<Indices...>) {
  (record_input_argument<std::decay_t<type_list_element_t<Indices, ArgList>>>{}(desc,
                                                                                args.at(Indices)),
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
}  // namespace wf
