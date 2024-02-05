// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <ostream>
#include <string>

#include "wf/expression_variant.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/fmt_imports.h"
#include "wf/operations.h"
#include "wf/ordering.h"

namespace wf {

// Empty type that we use to specify a list of types for `expression_variant`.
struct scalar_meta_type {};

template <>
struct type_list_trait<scalar_meta_type> {
  // All the scalar-valued expressions.
  // clang-format off
  using types = type_list<
    const class addition,
    const class cast_bool,
    const class compound_expression_element,
    const class conditional,
    const class symbolic_constant,
    const class derivative,
    const class float_constant,
    const class function,
    const class complex_infinity,
    const class integer_constant,
    const class multiplication,
    const class power,
    const class rational_constant,
    const class relational,
    const class undefined,
    const class variable
    >;
  // clang-format on
};

// An abstract scalar-valued expression.
class scalar_expr final : public expression_base<scalar_expr, scalar_meta_type> {
 public:
  using expression_base::expression_base;

  // Construct variable with name and specific numeric set:
  explicit scalar_expr(std::string_view name, number_set set = number_set::unknown);

  // Enable if the type `T` is a numeric type that we allow to be implicitly converted to
  // `scalar_expr`.
  template <typename T>
  using enable_if_supports_implicit_conversion =
      std::enable_if_t<(std::is_integral_v<T> && !std::is_same_v<T, bool>) ||
                       std::is_floating_point_v<T>>;

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T, typename = enable_if_supports_implicit_conversion<T>>
  scalar_expr(T v) : scalar_expr(construct_implicit(v)) {}

  // Construct from rational.
  explicit scalar_expr(rational_constant r);

  // Convert to string.
  std::string to_string() const;

  // Convert to string of the expression tree.
  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Negation operator.
  scalar_expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  scalar_expr diff(
      const scalar_expr& var, int reps = 1,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const {
    return wf::diff(*this, var, reps, behavior);
  }

  // Distribute terms in this expression.
  scalar_expr distribute() const { return wf::distribute(*this); }

  // Create a new expression by recursively substituting `replacement` for `target`.
  scalar_expr subs(const scalar_expr& target, const scalar_expr& replacement) const {
    return wf::substitute(*this, target, replacement);
  }

  // Create a new expression by recursively replacing [variable, replacement] pairs.
  scalar_expr substitute_variables(
      absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs) const {
    return wf::substitute_variables(*this, pairs);
  }

  // Collect terms in this expression.
  template <typename... Args>
  scalar_expr collect(Args&&... args) const {
    if constexpr (sizeof...(Args) == 1) {
      return wf::collect(*this, std::forward<Args>(args)...);
    } else {
      return wf::collect_many(*this, {std::forward<Args>(args)...});
    }
  }

  // Evaluate into float.
  scalar_expr eval() const { return wf::evaluate(*this); }

 protected:
  friend class matrix_expr;

  // Construct constant from float.
  static scalar_expr from_float(double x);

  // Construct from integer.
  static scalar_expr from_int(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static scalar_expr construct_implicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    if constexpr (std::is_integral_v<T>) {
      return from_int(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return from_float(static_cast<double>(v));
    }
  }
};

static_assert(std::is_nothrow_move_constructible_v<scalar_expr> &&
                  std::is_nothrow_move_assignable_v<scalar_expr>,
              "Should be movable");

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const scalar_expr& x) {
  stream << x.to_string();
  return stream;
}

// Math operators.
scalar_expr operator+(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator-(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator*(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator/(const scalar_expr& a, const scalar_expr& b);

// Comparison operators. These create relational expressions, rather than directly returning bool.
scalar_expr operator<(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator>(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator<=(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator>=(const scalar_expr& a, const scalar_expr& b);
scalar_expr operator==(const scalar_expr& a, const scalar_expr& b);

// Determine relative order of two expressions.
// This is not a mathematical ordering - rather it is a canonical ordering we impose on expressions.
template <>
struct order_struct<scalar_expr> {
  // Implemented in ordering.cc
  relative_order operator()(const scalar_expr& a, const scalar_expr& b) const;
};

// Predicate for sorting expressions.
struct expression_order_struct {
  bool operator()(const scalar_expr& a, const scalar_expr& b) const {
    return determine_order(a, b) == relative_order::less_than;
  }
};

// Get operation precedence (order of operations).
// Implemented in expression.cc
precedence get_precedence(const scalar_expr& expr);

// Custom literal suffix support.
namespace custom_literals {
inline scalar_expr operator"" _s(unsigned long long int arg) { return scalar_expr{arg}; }
inline scalar_expr operator"" _s(long double arg) { return scalar_expr{arg}; }
}  // namespace custom_literals

// Create a tuple of `scalar_expr` from string arguments.
template <typename... Args>
auto make_symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(scalar_expr{std::forward<Args>(args)}...);
}

// Make a unique variable symbol.
scalar_expr make_unique_variable_symbol(number_set set);

// Create an `scalar_expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
scalar_expr make_expr(Args&&... args) noexcept(noexcept(scalar_expr{std::in_place_type_t<T>{},
                                                                    std::forward<Args>(args)...})) {
  return scalar_expr{std::in_place_type_t<T>{}, std::forward<Args>(args)...};
}

}  // namespace wf

// libfmt support:
template <>
struct fmt::formatter<wf::scalar_expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::scalar_expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    // TODO: This could be implemented so as to avoid the string copy.
    //  It would require all types being visible at the formatter location though.
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
