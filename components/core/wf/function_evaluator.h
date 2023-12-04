// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/function_evaluator_detail.h"
#include "wf/type_annotations.h"

namespace math {

// Invoke the provided function `func` and capture all the output expressions.
// The outputs are inspected and converted into an instance of `ast::function_signature` and a
// vector of output expressions.
template <typename Func, typename... ArgumentInfo>
std::tuple<ast::function_signature, std::vector<expression_group>> build_function_description(
    Func&& func, const std::string_view function_name, ArgumentInfo&&... args_in) {
  static_assert(std::conjunction_v<std::is_constructible<arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");

  // Extract return type and argument list of the provided function pointer or lambda.
  using Traits = function_traits<Func>;
  using ArgList = typename Traits::args_list;  //  List of all argument types.
  static_assert(type_list_size_v<ArgList> == sizeof...(ArgumentInfo),
                "Mismatch in # args and # arg names");

  // Convert args into an array so that we can index them.
  const std::array<arg, sizeof...(ArgumentInfo)> args = {
      arg(std::forward<ArgumentInfo>(args_in))...};

  // Build inputs and invoke the function
  std::tuple outputs = detail::invoke_with_output_capture<ArgList>(
      std::forward<Func>(func), std::make_index_sequence<Traits::arity>());

  // Copy expressions into `expression_group` objects, one per output:
  std::vector<expression_group> groups{};
  detail::copy_output_expression_from_tuple(outputs, groups);

  // Add all the input arguments:
  ast::function_signature signature{std::string(function_name)};
  detail::record_input_args<ArgList>(signature, args, std::make_index_sequence<Traits::arity>());

  // Record all the output arguments:
  std::apply(
      [&](auto&&... output_expression) {
        static_assert(
            std::conjunction_v<
                is_output_arg_or_return_value<std::decay_t<decltype(output_expression)>>...>,
            "All returned elements of the tuple must be explicitly marked as `return_value` or "
            "`output_arg`.");
        (detail::record_output<std::decay_t<decltype(output_expression)>>{}(signature,
                                                                            output_expression),
         ...);
      },
      outputs);

  return std::make_tuple(std::move(signature), std::move(groups));
}

}  // namespace math
