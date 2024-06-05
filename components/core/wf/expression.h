// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <memory>
#include <ostream>  // operator<<
#include <string>

#include "wf/boolean_expression.h"
#include "wf/enumerations.h"
#include "wf/expression_variant.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/utility/ordering.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
#include <fmt/core.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Empty type that we use to specify a list of types for `expression_variant`.
struct scalar_meta_type {};

template <>
struct type_list_trait<scalar_meta_type> {
  // All the scalar-valued expressions.
  // clang-format off
  using types = type_list<
    class addition,
    class compound_expression_element,
    class conditional,
    class symbolic_constant,
    class derivative,
    class float_constant,
    class function,
    class complex_infinity,
    class imaginary_unit,
    class integer_constant,
    class iverson_bracket,
    class multiplication,
    class power,
    class rational_constant,
    class undefined,
    class unevaluated,
    class variable
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
      std::enable_if_t<std::is_constructible_v<checked_int, T> || std::is_floating_point_v<T>>;

  // Implicit construction from integers and floats.
  // ReSharper disable once CppNonExplicitConvertingConstructor
  template <typename T, typename = enable_if_supports_implicit_conversion<T>>
  scalar_expr(T v) : scalar_expr(construct_implicit(v)) {}

  // Construct from `rational_constant`. Special cased so we can simplify to integer.
  explicit scalar_expr(rational_constant r);

  // Construct from `float_constant`. Special cased so we can simplify to constants.
  explicit scalar_expr(float_constant f);

  // Construct from `integer_constant`. Special cased so we can simplify to constants.
  explicit scalar_expr(integer_constant i);

  // Static constructor: Conversion from complex float: a + i*b
  static scalar_expr from_complex(double a, double b);

  // Convert to string.
  std::string to_string() const;

  // Convert to string of the expression tree.
  std::string to_expression_tree_string() const;

  // Negation operator.
  scalar_expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  scalar_expr diff(
      const scalar_expr& var, int reps = 1,
      non_differentiable_behavior behavior = non_differentiable_behavior::constant) const;

  // Distribute terms in this expression.
  scalar_expr distribute() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  scalar_expr subs(const scalar_expr& target, const scalar_expr& replacement) const;

  // Collect terms in this expression.
  scalar_expr collect(const scalar_expr& term) const;
  scalar_expr collect(absl::Span<const scalar_expr> terms) const;

  // Evaluate into float expression.
  scalar_expr eval() const;

 protected:
  friend class matrix_expr;

  // Construct constant from float.
  static scalar_expr from_float(double x);

  // Construct from integer.
  static scalar_expr from_int(checked_int x);

  template <typename T>
  static scalar_expr construct_implicit(T v) {
    static_assert(std::is_constructible_v<checked_int, T> || std::is_floating_point_v<T>);
    if constexpr (std::is_constructible_v<checked_int, T>) {
      return from_int(static_cast<checked_int>(v));
    } else {
      return from_float(static_cast<double>(v));
    }
  }
};

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

// Comparison operators. These create relational expressions.
boolean_expr operator<(const scalar_expr& a, const scalar_expr& b);
boolean_expr operator>(const scalar_expr& a, const scalar_expr& b);
boolean_expr operator<=(const scalar_expr& a, const scalar_expr& b);
boolean_expr operator>=(const scalar_expr& a, const scalar_expr& b);
boolean_expr operator==(const scalar_expr& a, const scalar_expr& b);

// Determine relative order of two scalar expressions.
// This is not a mathematical ordering - rather it is a canonical ordering we impose on expressions.
template <>
struct order_struct<scalar_expr> {
  relative_order operator()(const scalar_expr& a, const scalar_expr& b) const {
    if (a.has_same_address(b)) {
      return relative_order::equal;
    }
    return compare(a, b);
  }

 private:
  relative_order compare(const scalar_expr& a, const scalar_expr& b) const;
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
inline scalar_expr operator"" _s(const unsigned long long int arg) {
  return scalar_expr{checked_int::from_unsigned_long_long(arg)};
}
inline scalar_expr operator"" _s(const long double arg) { return scalar_expr{arg}; }
}  // namespace custom_literals

// Create a tuple of `scalar_expr` from string arguments.
template <typename... Args>
auto make_symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(scalar_expr{std::forward<Args>(args), number_set::unknown}...);
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

// libfmt support for scalar_expr.
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
