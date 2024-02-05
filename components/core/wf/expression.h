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
class Expr final : public expression_base<Expr, scalar_meta_type> {
 public:
  using expression_base::expression_base;

  // Construct variable with name and specific numeric set:
  explicit Expr(std::string_view name, number_set set = number_set::unknown);

  // Enable if the type `T` is a numeric type that we allow to be implicitly converted to `Expr`.
  template <typename T>
  using enable_if_supports_implicit_conversion =
      std::enable_if_t<(std::is_integral_v<T> && !std::is_same_v<T, bool>) ||
                       std::is_floating_point_v<T>>;

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T, typename = enable_if_supports_implicit_conversion<T>>
  Expr(T v) : Expr(construct_implicit(v)) {}

  // Construct from rational.
  explicit Expr(rational_constant r);

  // Convert to string.
  std::string to_string() const;

  // Convert to string of the expression tree.
  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr diff(const Expr& var, int reps = 1,
            non_differentiable_behavior behavior = non_differentiable_behavior::constant) const {
    return wf::diff(*this, var, reps, behavior);
  }

  // Distribute terms in this expression.
  Expr distribute() const { return wf::distribute(*this); }

  // Create a new expression by recursively substituting `replacement` for `target`.
  Expr subs(const Expr& target, const Expr& replacement) const {
    return wf::substitute(*this, target, replacement);
  }

  // Create a new expression by recursively replacing [variable, replacement] pairs.
  Expr substitute_variables(absl::Span<const std::tuple<Expr, Expr>> pairs) const {
    return wf::substitute_variables(*this, pairs);
  }

  // Collect terms in this expression.
  template <typename... Args>
  Expr collect(Args&&... args) const {
    if constexpr (sizeof...(Args) == 1) {
      return wf::collect(*this, std::forward<Args>(args)...);
    } else {
      return wf::collect_many(*this, {std::forward<Args>(args)...});
    }
  }

  // Evaluate into float.
  Expr eval() const { return wf::evaluate(*this); }

 protected:
  friend class matrix_expr;

  // Construct constant from float.
  static Expr from_float(double x);

  // Construct from integer.
  static Expr from_int(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr construct_implicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    if constexpr (std::is_integral_v<T>) {
      return from_int(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return from_float(static_cast<double>(v));
    }
  }
};

static_assert(std::is_nothrow_move_constructible_v<Expr> && std::is_nothrow_move_assignable_v<Expr>,
              "Should be movable");

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.to_string();
  return stream;
}

// Math operators.
Expr operator+(const Expr& a, const Expr& b);
Expr operator-(const Expr& a, const Expr& b);
Expr operator*(const Expr& a, const Expr& b);
Expr operator/(const Expr& a, const Expr& b);

// Comparison operators. These create relational expressions, rather than directly returning bool.
Expr operator<(const Expr& a, const Expr& b);
Expr operator>(const Expr& a, const Expr& b);
Expr operator<=(const Expr& a, const Expr& b);
Expr operator>=(const Expr& a, const Expr& b);
Expr operator==(const Expr& a, const Expr& b);

// Determine relative order of two expressions.
// This is not a mathematical ordering - rather it is a canonical ordering we impose on expressions.
template <>
struct order_struct<Expr> {
  // Implemented in ordering.cc
  relative_order operator()(const Expr& a, const Expr& b) const;
};

// Predicate for sorting expressions.
struct expression_order_struct {
  bool operator()(const Expr& a, const Expr& b) const {
    return determine_order(a, b) == relative_order::less_than;
  }
};

// Get operation precedence (order of operations).
// Implemented in expression.cc
precedence get_precedence(const Expr& expr);

// Custom literal suffix support.
namespace custom_literals {
inline Expr operator"" _s(unsigned long long int arg) { return Expr{arg}; }
inline Expr operator"" _s(long double arg) { return Expr{arg}; }
}  // namespace custom_literals

// Create a tuple of `Expr` from string arguments.
template <typename... Args>
auto make_symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(Expr{std::forward<Args>(args)}...);
}

// Make a unique variable symbol.
Expr make_unique_variable_symbol(number_set set);

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr make_expr(Args&&... args) noexcept(noexcept(Expr{std::in_place_type_t<T>{},
                                                      std::forward<Args>(args)...})) {
  return Expr{std::in_place_type_t<T>{}, std::forward<Args>(args)...};
}

}  // namespace wf

// libfmt support:
template <>
struct fmt::formatter<wf::Expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::Expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    // TODO: This could be implemented so as to avoid the string copy.
    //  It would require all types being visible at the formatter location though.
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
