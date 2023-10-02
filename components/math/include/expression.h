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
  explicit Expr(ExpressionConceptConstPtr&& impl) : impl_(std::move(impl)) {}
  explicit Expr(const ExpressionConceptConstPtr& impl) : impl_(impl) {}

  // Construct variable:
  explicit Expr(std::string_view name);

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T,
            typename = std::enable_if_t<(std::is_integral_v<T> && !std::is_same_v<T, bool>) ||
                                        std::is_floating_point_v<T>>>
  Expr(T v) : Expr(ConstructImplicit(v)) {}

  // Test if the two expressions are identical.
  bool is_identical_to(const Expr& other) const { return impl_->is_identical_to(*other.impl_); }

  // Check if the underlying expression is one of the specified types.
  template <typename... Ts>
  bool Is() const {
    return impl_->IsType<Ts...>();
  }

  // Test if the two expressions have the same underlying address.
  bool HasSameAddress(const Expr& other) const { return impl_.get() == other.impl_.get(); }

  // Useful for debugging sometimes: get the underlying address as void*.
  [[maybe_unused]] const void* GetAddress() const { return static_cast<const void*>(impl_.get()); }

  // Get the underlying type name as a string.
  std::string_view TypeName() const { return impl_->TypeName(); }

  // Return the unique index of the underlying type.
  std::size_t TypeIndex() const { return impl_->TypeIndex(); }

  // Whether this expression is a leaf node in the expression tree.
  bool IsLeaf() const { return impl_->IsLeaf(); }

  // Convert to string.
  std::string ToString() const;

  // Convert to string of the expression tree.
  // Defined in tree_formatter.cc
  std::string ToExpressionTreeString() const;

  // Get the hash of the expression.
  std::size_t Hash() const { return impl_->GetHash(); }

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int reps = 1) const { return math::Diff(*this, var, reps); }

  // Distribute terms in this expression.
  Expr Distribute() const { return math::Distribute(*this); }

  // Create a new expression by recursively substituting `replacement` for `target`.
  Expr Subs(const Expr& target, const Expr& replacement) const {
    return math::Substitute(*this, target, replacement);
  }

  // Collect terms in this expression.
  template <typename... Args>
  Expr Collect(Args&&... args) const {
    if constexpr (sizeof...(Args) == 1) {
      return math::Collect(*this, std::forward<Args>(args)...);
    } else {
      return math::CollectMany(*this, {std::forward<Args>(args)...});
    }
  }

  // Evaluate into float.
  Expr Eval() const { return math::Eval(*this); }

  // Receive a visitor.
  void Receive(VisitorBase& visitor) const { impl_->Receive(visitor); }

 protected:
  [[nodiscard]] const ExpressionConceptConstPtr& Impl() const { return impl_; }

  friend class MatrixExpr;

  template <typename T>
  friend const T* CastPtr(const Expr&);
  template <typename T>
  friend const T& CastChecked(const Expr&);
  template <typename T>
  friend const T& CastUnchecked(const Expr& x);

 private:
  // Construct constant from float.
  static Expr FromFloat(double x);

  // Construct from integer.
  static Expr FromInt(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr ConstructImplicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    static_assert(!std::is_same_v<T, bool>);
    if constexpr (std::is_integral_v<T>) {
      return FromInt(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return FromFloat(static_cast<double>(v));
    }
  }

  ExpressionConceptConstPtr impl_;
};

static_assert(std::is_nothrow_move_constructible_v<Expr> && std::is_nothrow_move_assignable_v<Expr>,
              "Should be movable");

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToString();
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
RelativeOrder ExpressionOrder(const Expr& a, const Expr& b);

// Predicate for sorting expressions.
struct ExpressionOrderPredicate {
  bool operator()(const Expr& a, const Expr& b) const {
    return ExpressionOrder(a, b) == RelativeOrder::LessThan;
  }
};

// Get operation precedence (order of operations).
// Implemented in expression.cc
Precedence GetPrecedence(const Expr& expr);

// Check for strict equality. For use in template parameter lists for maps and sets.
template <typename T>
struct IsIdenticalOperator {
  bool operator()(const T& a, const T& b) const { return a.is_identical_to(b); }
};

// Custom literal suffix support.
namespace custom_literals {
inline Expr operator"" _s(unsigned long long int arg) { return Expr{arg}; }
inline Expr operator"" _s(long double arg) { return Expr{arg}; }
}  // namespace custom_literals

// Create a tuple of `Expr` from string arguments.
template <typename... Args>
auto Symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(Expr{std::forward<Args>(args)}...);
}

}  // namespace math

// libfmt support:
template <>
struct fmt::formatter<math::Expr> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Expr& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    // TODO: This could be implemented so as to avoid the string copy.
    //  It would require all types being visible at the formatter location though.
    return fmt::format_to(ctx.out(), "{}", x.ToString());
  }
};
