// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/function_description.h"
#include "wf/output_annotations.h"
#include "wf/template_utils.h"

namespace wf {

namespace detail {

// Invoke the provided callable and return the output expressions. First builds a tuple of input
// arguments by constructing `function_argument` variable expressions for every input arg of
// `callable`. The resulting expressions are returned as a tuple of `output_arg<>` or
// `return_value<>`.
template <typename Callable, typename... ArgTypes>
auto invoke_with_symbolic_inputs(Callable&& callable, const std::tuple<ArgTypes...>& arg_types) {
  // Create a tuple of arguments w/ variable expressions.
  auto args = zip_enumerate_tuples(
      [](auto index, const auto& arg_type) { return create_function_input(arg_type, index()); },
      arg_types);

  // Call the user provided function with the args we just created:
  auto return_expr = std::apply(
      [&](auto&&... args_unpacked) {
        return std::invoke(std::forward<Callable>(callable),
                           std::forward<decltype(args_unpacked)>(args_unpacked)...);
      },
      std::move(args));

  using return_type = decltype(return_expr);
  static_assert(!std::is_same_v<return_type, void>, "Return type should not be void.");

  if constexpr (is_tuple_v<return_type>) {
    return return_expr;
  } else if constexpr (is_return_value<return_type>::value) {
    return std::make_tuple(std::move(return_expr));
  } else {
    return std::make_tuple(return_value(std::move(return_expr)));
  }
}

}  // namespace detail
}  // namespace wf
