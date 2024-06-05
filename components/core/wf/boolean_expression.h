// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression_variant.h"
#include "wf/utility/ordering.h"

namespace wf {

struct boolean_meta_type {};
template <>
struct type_list_trait<boolean_meta_type> {
  // All the boolean-valued expressions.
  // clang-format off
  using types = type_list<
    class boolean_constant,
    class relational
    >;
  // clang-format on
};

// An abstract boolean-valued expression.
class boolean_expr final : public expression_base<boolean_expr, boolean_meta_type> {
 public:
  using expression_base::expression_base;

  // Support implicit construction from bool (and only bool).
  // ReSharper disable once CppNonExplicitConvertingConstructor
  template <typename T, typename = std::enable_if_t<std::is_same_v<bool, T>>>
  boolean_expr(const T value) : boolean_expr(construct_implicit(value)) {}

  // Convert to string.
  std::string to_string() const;

  // Convert to graphical expression tree.
  std::string to_expression_tree_string() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  boolean_expr subs(const class scalar_expr& a, const scalar_expr& b) const;

 private:
  static boolean_expr construct_implicit(bool value);
};

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const boolean_expr& x) {
  stream << x.to_string();
  return stream;
}

// Determine relative order of two boolean expressions.
// This is not a mathematical ordering - rather it is a canonical ordering we impose on expressions.
template <>
struct order_struct<boolean_expr> {
  relative_order operator()(const boolean_expr& a, const boolean_expr& b) const;
};

}  // namespace wf

// libfmt support for boolean_expr.
template <>
struct fmt::formatter<wf::boolean_expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::boolean_expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
