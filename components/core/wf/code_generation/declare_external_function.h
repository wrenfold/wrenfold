// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/type_registry.h"
#include "wf/expressions/external_function_invocation.h"
#include "wf/external_function.h"
#include "wf/type_annotations.h"
#include "wf/utility/zip_tuples.h"

namespace wf {

namespace detail {
// Helper that declares a `call()` function with arguments specified by a type list.
template <typename Derived, typename Args>
class implement_call;
template <typename Derived, typename... Ts>
class implement_call<Derived, type_list<Ts...>> {
 public:
  static auto call(Ts... args) { return Derived::call_internal(std::move(args)...); }
};
}  // namespace detail

// Base type to declare external user-defined functions in C++.
// This is mostly defined as a convenience for implementing unit tests.
template <typename Derived, typename ReturnType, typename ArgTypes>
class declare_external_function {
 public:
  static_assert(!std::is_same_v<void, ReturnType>, "Return type cannot be void.");

  template <typename... Args>
  static auto call(Args... args) {
    static_assert(sizeof...(args) == type_list_size_v<ArgTypes>, "Wrong number of arguments");

    // On first invocation, we build a runtime description of the function.
    const auto& description_and_arg_types = get_description_and_arg_types();
    const external_function& description = std::get<0>(description_and_arg_types);

    // Convert all the args to something we can store in `external_function_invocation`.
    external_function_invocation::container_type captured_args{};
    captured_args.reserve(sizeof...(args));
    zip_tuples(
        [&captured_args](auto arg, const auto& arg_type) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, scalar_expr> || std::is_same_v<T, matrix_expr> ||
                        type_annotations::is_static_matrix_v<T>) {
            captured_args.emplace_back(std::move(arg));
          } else if constexpr (std::is_constructible_v<scalar_expr, T>) {
            captured_args.emplace_back(scalar_expr{arg});
          } else if constexpr (implements_custom_type_registrant_v<T>) {
            // This was directly constructed - capture all the expressions on the custom type.
            captured_args.push_back(detail::extract_function_output(arg_type, arg));
          } else {
            WF_ASSERT_ALWAYS("Unsupported argument type: {}", typeid(T).name());
          }
        },
        std::forward_as_tuple(std::move(args)...), std::get<1>(description_and_arg_types));

    auto invoke_result = description.create_invocation(std::move(captured_args));

    // We construct different expression types, depending on what the external function returns:
    if constexpr (std::is_same_v<ReturnType, scalar_expr>) {
      // Get single element from the compound expression:
      return ReturnType{std::get<scalar_expr>(invoke_result)};
    } else if constexpr (std::is_same_v<ReturnType, matrix_expr> ||
                         type_annotations::is_static_matrix_v<ReturnType>) {
      return ReturnType{std::get<matrix_expr>(invoke_result)};
    } else if constexpr (implements_custom_type_registrant_v<ReturnType>) {
      const custom_type& type = description.return_type_as<custom_type>();
      const std::vector<scalar_expr> elements =
          create_expression_elements(std::get<compound_expr>(invoke_result), type.total_size());
      auto [return_value, _] =
          custom_type_from_expressions<ReturnType>(type, absl::Span<const scalar_expr>{elements});
      return return_value;
    } else {
      WF_ASSERT_ALWAYS("Unsupported return type: {}", typeid(ReturnType).name());
    }
  }

  // Get the function description.
  static const auto& get_description_and_arg_types() {
    static const auto description = create_description();
    return description;
  }

 private:
  // Create a runtime description of this function from the signature information
  // we scrape from the derived type. This will be invoked once, and then passed into
  // invocations.
  static auto create_description() {
    const std::tuple arg_names = Derived::arg_names();
    static_assert(
        std::tuple_size_v<std::remove_const_t<decltype(arg_names)>> == type_list_size_v<ArgTypes>,
        "Length of arg_names() must match length of ArgTypes.");

    custom_type_registry registry{};
    std::tuple arg_types = detail::record_arg_types(registry, ArgTypes{});

    // Iterate over arguments and record them:
    std::vector<argument> arguments{};
    arguments.reserve(type_list_size_v<ArgTypes>);
    zip_enumerate_tuples(
        [&](auto index, const std::string_view name, const auto& arg_type) {
          // TODO: Support output arguments? Will entail a bit more complexity in the
          //  code-generation step.
          arguments.emplace_back(name, arg_type, argument_direction::input, index());
        },
        arg_names, arg_types);

    // Record the return type (we allow only one for now, but we could support tuples):
    auto [return_type] = detail::record_arg_types(registry, type_list<ReturnType>{});

    // Return the description of the external function:
    return std::make_tuple(
        external_function(std::string{Derived::name()}, std::move(arguments), return_type),
        std::move(arg_types));
  }
};

}  // namespace wf
