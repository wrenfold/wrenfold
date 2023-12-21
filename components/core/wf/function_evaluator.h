// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/function_evaluator_detail.h"
#include "wf/type_annotations.h"

namespace wf {

// Invoke the provided function `func` and capture all the output expressions.
// The outputs are inspected and converted into an instance of `ast::function_signature` and a
// vector of output expressions.
template <typename Func, typename... ArgumentInfo>
function_description build_function_description(Func&& func, const std::string_view function_name,
                                                ArgumentInfo&&... args_in) {
  static_assert(std::conjunction_v<std::is_constructible<arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");

  // Extract return type and argument list of the provided function pointer or lambda.
  using Traits = function_traits<Func>;
  using ArgList = typename Traits::args_list;  //  List of all argument types.
  static_assert(type_list_size_v<ArgList> == sizeof...(ArgumentInfo),
                "Mismatch in # args and # arg names");

  // Convert args into an array so that we can index them.
  const std::vector<arg> args = {arg(std::forward<ArgumentInfo>(args_in))...};

  // Build inputs and invoke the function
  const std::tuple outputs = detail::invoke_with_output_capture<ArgList>(
      std::forward<Func>(func), std::make_index_sequence<Traits::arity>());

  // Add all the input arguments:
  function_description description{std::string(function_name)};
  detail::record_input_args<ArgList>(description, args, std::make_index_sequence<Traits::arity>());

  // Record all the output arguments:
  std::apply(
      [&](auto&&... output_expression) {
        static_assert(
            std::conjunction_v<
                is_output_arg_or_return_value<std::decay_t<decltype(output_expression)>>...>,
            "All returned elements of the tuple must be explicitly marked as `return_value` or "
            "`output_arg`.");
        (detail::record_output<std::decay_t<decltype(output_expression)>>{}(description,
                                                                            output_expression),
         ...);
      },
      outputs);

  return description;
}

}  // namespace wf
