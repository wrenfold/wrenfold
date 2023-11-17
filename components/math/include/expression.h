// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <ostream>
#include <string>

#include "expression_concept.h"
#include "fmt_imports.h"
#include "operations.h"

namespace math {

/**
 * Wrapper around a pointer to an abstract expression. Defined so you can easily write chains of
 * operations without dealing with pointers at all.
 */
class Expr {
 public:
  // Constructors.
  explicit Expr(expression_concept_const_ptr&& impl) : impl_(std::move(impl)) {}
  explicit Expr(const expression_concept_const_ptr& impl) : impl_(impl) {}

  // Construct variable with name and specific numeric set:
  explicit Expr(std::string_view name, NumberSet set = NumberSet::Unknown);

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T,
            typename = std::enable_if_t<(std::is_integral_v<T> && !std::is_same_v<T, bool>) ||
                                        std::is_floating_point_v<T>>>
  Expr(T v) : Expr(construct_implicit(v)) {}

  // Test if the two expressions have the same underlying address.
  bool has_same_address(const Expr& other) const noexcept {
    return impl_.get() == other.impl_.get();
  }

  // Test if the two expressions are identical.
  bool is_identical_to(const Expr& other) const {
    return has_same_address(other) || impl_->is_identical_to(*other.impl_);
  }

  // Check if the underlying expression is one of the specified types.
  template <typename... Ts>
  bool is_type() const noexcept {
    return impl_->is_type<Ts...>();
  }

  // Useful for debugging sometimes: get the underlying address as void*.
  [[maybe_unused]] const void* get_address() const noexcept {
    return static_cast<const void*>(impl_.get());
  }

  // Get the underlying type name as a string.
  std::string_view type_name() const { return impl_->type_name(); }

  // Return the unique index of the underlying type.
  std::size_t type_index() const { return impl_->type_index(); }

  // Whether this expression is a leaf node in the expression tree.
  bool is_leaf() const { return impl_->is_leaf(); }

  // Convert to string.
  std::string to_string() const;

  // Convert to string of the expression tree.
  // Defined in tree_formatter.cc
  std::string to_expression_tree_string() const;

  // Get the hash of the expression.
  std::size_t get_hash() const { return impl_->get_hash(); }

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr diff(const Expr& var, int reps = 1) const { return math::diff(*this, var, reps); }

  // Distribute terms in this expression.
  Expr distribute() const { return math::distribute(*this); }

  // Create a new expression by recursively substituting `replacement` for `target`.
  Expr subs(const Expr& target, const Expr& replacement) const {
    return math::substitute(*this, target, replacement);
  }

  // Create a new expression by recursively replacing [variable, replacement] pairs.
  Expr substitute_variables(absl::Span<const std::tuple<Expr, Expr>> pairs) const {
    return math::substitute_variables(*this, pairs);
  }

  // Collect terms in this expression.
  template <typename... Args>
  Expr collect(Args&&... args) const {
    if constexpr (sizeof...(Args) == 1) {
      return math::collect(*this, std::forward<Args>(args)...);
    } else {
      return math::collect_many(*this, {std::forward<Args>(args)...});
    }
  }

  // Evaluate into float.
  Expr eval() const { return math::evaluate(*this); }

 protected:
  [[nodiscard]] const expression_concept_const_ptr& Impl() const { return impl_; }

  friend class MatrixExpr;

  template <typename T>
  friend const T* cast_ptr(const Expr&);
  template <typename T>
  friend const T& cast_checked(const Expr&);
  template <typename T>
  friend const T& cast_unchecked(const Expr& x);

 private:
  // Construct constant from float.
  static Expr from_float(double x);

  // Construct from integer.
  static Expr from_int(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr construct_implicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    static_assert(!std::is_same_v<T, bool>);
    if constexpr (std::is_integral_v<T>) {
      return from_int(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return from_float(static_cast<double>(v));
    }
  }

  expression_concept_const_ptr impl_;
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

// Determine relative order of two expressions (for sorting).
// Can be used to sort expressions into a canonical order, for instance to sort them.
// Implemented in ordering.cc
RelativeOrder expression_order(const Expr& a, const Expr& b);

// Predicate for sorting expressions.
struct ExpressionOrderPredicate {
  bool operator()(const Expr& a, const Expr& b) const {
    return expression_order(a, b) == RelativeOrder::LessThan;
  }
};

// Get operation precedence (order of operations).
// Implemented in expression.cc
Precedence get_precedence(const Expr& expr);

// Check for strict equality. For use in template parameter lists for maps and sets.
template <typename T>
struct is_identical_struct {
  bool operator()(const T& a, const T& b) const { return a.is_identical_to(b); }
};

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
Expr make_unique_variable_symbol(NumberSet set);

}  // namespace math

// libfmt support:
template <>
struct fmt::formatter<math::Expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    // TODO: This could be implemented so as to avoid the string copy.
    //  It would require all types being visible at the formatter location though.
    return fmt::format_to(ctx.out(), "{}", x.to_string());
  }
};
