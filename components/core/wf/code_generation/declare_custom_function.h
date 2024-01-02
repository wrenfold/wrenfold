#pragma once
#include "wf/code_generation/custom_function.h"
#include "wf/expressions/custom_function_invocation.h"
#include "wf/index_range.h"

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
template <typename Derived>
class declare_custom_function : protected detail::implement_call<declare_custom_function<Derived>,
                                                                 typename Derived::arg_types> {
 public:
  using derived_return_type = typename Derived::return_type;
  using derived_arg_types = typename Derived::arg_types;
  using base = detail::implement_call<declare_custom_function, derived_arg_types>;

  static_assert(!std::is_same_v<void, derived_return_type>, "Return type cannot be void (yet).");
  static_assert(std::tuple_size_v<std::decay_t<decltype(Derived::arg_names)>> ==
                    type_list_size_v<derived_arg_types>,
                "Length of arg_names must match length of arg_types.");

 private:
  friend class base;

  template <typename... Args>
  static auto call_internal(Args... args) {
    // On first incovation, we build a runtime description of the function.
    static const std::tuple description_and_arg_types = create_description();
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
          } else if constexpr (implements_custom_type_registrant_v<T>) {
            std::vector<Expr> struct_expressions = detail::extract_function_output(arg_type, arg);
            captured_args.emplace_back(
                compound_expr(std::in_place_type_t<custom_type_construction>{}, arg_type,
                              std::move(struct_expressions)));
          }
        },
        std::forward_as_tuple(std::move(args)...), std::get<1>(description_and_arg_types));

    compound_expr invocation{std::in_place_type_t<custom_function_invocation>{}, description,
                             std::move(captured_args)};

    // We consctruct different expression types, depending on what the custom function returns:
    if constexpr (std::is_same_v<derived_return_type, Expr>) {
      // Get single element from the compound expression:
      return Expr{std::in_place_type_t<compound_expression_element>{}, std::move(invocation), 0};
    } else if constexpr (std::is_same_v<derived_return_type, MatrixExpr> ||
                         type_annotations::is_static_matrix_v<derived_return_type>) {
      // Return a matrix built of compound expression elements:
      WF_ASSERT(std::holds_alternative<matrix_type>(description.return_type()),
                "Return type must be matrix_type. Function: `{}`", description.name());
      const matrix_type& mat = std::get<matrix_type>(description.return_type());

      std::vector<Expr> elements{};
      elements.reserve(mat.size());
      for (const std::size_t index : make_range(mat.size())) {
        elements.emplace_back(std::in_place_type_t<compound_expression_element>{},
                              std::move(invocation), index);
      }
      return MatrixExpr::create(mat.rows(), mat.cols(), std::move(elements));
    } else {
      return invocation;
    }
  }

  // Create a runtime description of this function from the signature information
  // we scrape from the derived type. This will be invoked once, and then passed into
  // invocations.
  static auto create_description() {
    constexpr std::tuple arg_names = Derived::arg_names;

    custom_type_registry registry{};
    std::tuple arg_types = detail::record_arg_types(registry, derived_arg_types{});

    // Iterate over arguments and record them:
    std::vector<argument> arguments{};
    arguments.reserve(type_list_size_v<derived_arg_types>);
    zip_enumerate_tuples(
        [&](auto index, const std::string_view name, const auto& arg_type) {
          // TODO: Support output arguments? Will entail a bit more complexity in the
          //  code-generation step.
          arguments.emplace_back(name, arg_type, argument_direction::input, index());
        },
        arg_names, arg_types);

    // Record the return type (we allow only one for now, but we could support tuples):
    auto [return_type] = detail::record_arg_types(registry, type_list<derived_return_type>{});

    // Return the description of the custom function:
    return std::make_tuple(
        custom_function(std::string{Derived::name}, std::move(arguments), return_type),
        std::move(arg_types));
  }
};

//
class my_custom_func : public declare_custom_function<my_custom_func> {
 public:
  static constexpr std::string_view name = "my_custom_func";
  using return_type = Expr;
  using arg_types = type_list<Expr, type_annotations::static_matrix<2, 2>>;
  static constexpr auto arg_names = std::make_tuple("foo", "bar");
};

}  // namespace wf
