#pragma once
#include "wf/code_generation/custom_function.h"
#include "wf/expressions/custom_function_invocation.h"

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
template <typename Derived, typename ReturnType, typename ArgTypes>
class declare_custom_function {
 public:
  static_assert(!std::is_same_v<void, ReturnType>, "Return type cannot be void.");

  template <typename... Args>
  static auto call(Args... args) {
    static_assert(sizeof...(args) == type_list_size_v<ArgTypes>, "Wrong number of arguments");

    // On first incovation, we build a runtime description of the function.
    const auto& description_and_arg_types = get_description_and_arg_types();
    const custom_function& description = std::get<0>(description_and_arg_types);

    // Convert all the args to something we can store in `custom_function_invocation`.
    custom_function_invocation::container_type captured_args{};
    captured_args.reserve(sizeof...(args));
    zip_tuples(
        [&captured_args](auto arg, const auto& arg_type) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, Expr> || std::is_same_v<T, MatrixExpr> ||
                        type_annotations::is_static_matrix_v<T>) {
            captured_args.emplace_back(std::move(arg));
          } else if constexpr (detail::inherits_custom_type_base_v<T>) {
            if (const std::optional<compound_expr>& provenance = arg.provenance();
                provenance.has_value()) {
              // This custom object already has a provenance expression.
              captured_args.push_back(*provenance);
            } else {
              // This was directly constructed - capture all the expressions on the custom type.
              std::vector<Expr> struct_expressions = detail::extract_function_output(arg_type, arg);
              captured_args.emplace_back(std::in_place_type_t<compound_expr>{},
                                         std::in_place_type_t<custom_type_construction>{},
                                         arg_type.inner(), std::move(struct_expressions));
            }
          } else {
            WF_ASSERT_ALWAYS("Unsupported argument type: {}", typeid(T).name());
          }
        },
        std::forward_as_tuple(std::move(args)...), std::get<1>(description_and_arg_types));

    auto invoke_result = description.create_invocation(std::move(captured_args));

    // We construct different expression types, depending on what the custom function returns:
    if constexpr (std::is_same_v<ReturnType, Expr>) {
      // Get single element from the compound expression:
      return ReturnType{std::get<Expr>(invoke_result)};
    } else if constexpr (std::is_same_v<ReturnType, MatrixExpr> ||
                         type_annotations::is_static_matrix_v<ReturnType>) {
      return ReturnType{std::get<MatrixExpr>(invoke_result)};
    } else if constexpr (detail::inherits_custom_type_base_v<ReturnType>) {
      const custom_type& type = description.return_type_as<custom_type>();
      const std::vector<Expr> elements =
          create_expression_elements(std::get<compound_expr>(invoke_result), type.total_size());
      auto [return_value, _] =
          custom_type_from_expressions<ReturnType>(type, absl::Span<const Expr>{elements});
      // Record provenance of this object for use in downstream function invocations.
      return_value.set_provenance(std::get<compound_expr>(invoke_result));
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

    // Return the description of the custom function:
    return std::make_tuple(
        custom_function(std::string{Derived::name()}, std::move(arguments), return_type),
        std::move(arg_types));
  }
};

}  // namespace wf