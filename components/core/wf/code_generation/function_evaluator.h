// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/function_evaluator_detail.h"
#include "wf/type_annotations.h"

namespace wf {

// Invoke the provided function `func` and capture all the output expressions.
// The outputs are inspected and converted into an instance of `ast::function_signature` and a
// vector of output expressions.
template <typename Func, typename... ArgumentInfo>
function_description build_function_description(Func&& func, const std::string_view function_name,
                                                ArgumentInfo&&... args_in) {
  static_assert(std::conjunction_v<std::is_constructible<arg, ArgumentInfo>...>,
                "args_in must be convertible to type `wf::arg`.");

  // Extract return type and argument list of the provided function pointer or lambda.
  using func_traits = function_traits<Func>;
  using arg_type_list = typename func_traits::args_list;  //  List of all argument types.
  static_assert(type_list_size_v<arg_type_list> == sizeof...(ArgumentInfo),
                "Mismatch in # args and # arg names");

  // Convert arg names into an array so that we can index them.
  const std::tuple arg_names = std::make_tuple(arg(std::forward<ArgumentInfo>(args_in))...);

  // Scrape all the argument type information:
  custom_type_registry registry{};
  std::tuple arg_types = detail::record_arg_types(registry, arg_type_list{});

  // Build inputs and invoke the function
  std::tuple outputs = detail::invoke_with_symbolic_inputs(std::forward<Func>(func), arg_types);

  // Scrape all the output type information:
  std::tuple output_types =
      detail::record_arg_types(registry, type_list_from_tuple_t<decltype(outputs)>{});

  // Add all the input arguments:
  function_description description{std::string(function_name)};
  zip_tuples(
      [&description](type_variant arg_type, const arg& arg_name) {
        description.add_input_argument(arg_name.name(), std::move(arg_type));
      },
      std::move(arg_types), arg_names);

  // Record all the output arguments:
  zip_tuples(
      [&description](const auto& output_value, auto output_type) {
        static_assert(
            is_output_arg_or_return_value<std::decay_t<decltype(output_value)>>::value,
            "All returned elements of the tuple must be explicitly marked as `return_value` or "
            "`output_arg`.");
        description.add_output_value(output_value, std::move(output_type));
      },
      outputs, std::move(output_types));

  return description;
}

}  // namespace wf
