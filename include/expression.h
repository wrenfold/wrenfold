#pragma once
#include <memory>
#include <string>

#include "expression_concept.h"

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

  // Construct constant from float.
  static Expr FromFloat(double x);

  // Construct from integer.
  static Expr FromInt(std::int64_t x);

  // Get the implementation pointer.
  const ExpressionConceptConstPtr& GetImpl() const { return impl_; }

  // Get a raw pointer, cast dynamically to a particular type.
  template <typename T>
  const T* DynamicCast() const {
    return impl_->As<T>();
  }

  // Check that underlying type matches `T`.
  template <typename T>
  bool Is() const {
    return DynamicCast<T>() != nullptr;
  }

  // Static cast to the specified type.
  template <typename T>
  const T* StaticCast() const {
    return static_cast<const T*>(impl_.get());
  }

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const Expr& other) const { return impl_->IsIdenticalTo(other.impl_); }

  // Convert to string.
  std::string ToString() const;

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int reps = 1) const;

  // Distribute terms in this expression.
  Expr Distribute() const;

  // Receive a visitor.
  void Receive(VisitorBase<void>& visitor) const { impl_->Receive(visitor); }

  // Receive a visitor that returns an expression.
  Expr Receive(VisitorBase<Expr>& visitor) const { return impl_->Receive(visitor); }

 private:
  ExpressionConceptConstPtr impl_;
};

static_assert(std::is_move_assignable_v<Expr> && std::is_move_constructible_v<Expr>,
              "Should be movable");

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToString();
  return stream;
}

// Trait that we can implement to allow implicit conversions to `Expr`.
template <typename T>
struct CastToExpr;

template <>
struct CastToExpr<std::int64_t> {
  Expr operator()(std::int64_t v) const { return Expr::FromInt(v); }
};
template <>
struct CastToExpr<std::int32_t> {
  Expr operator()(std::int32_t v) const { return Expr::FromInt(v); }
};

// Evaluates to true for types which can be cast to `Expr`.
template <typename T, typename = void>
constexpr bool IsCastableToExpr = false;
template <typename T>
constexpr bool IsCastableToExpr<
    T, decltype(CastToExpr<std::decay_t<T>>{}(std::declval<std::decay_t<T>>()), void())> = true;

// Shorthand for CastToExpr.
template <typename T>
Expr to_expr_cast(T&& arg) {
  return CastToExpr<std::decay_t<T>>{}(std::forward<T>(arg));
}

// Math operators.
Expr operator+(const Expr& a, const Expr& b);
Expr operator-(const Expr& a, const Expr& b);
Expr operator*(const Expr& a, const Expr& b);
Expr operator/(const Expr& a, const Expr& b);

// Operator overloads involving primitives.
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator+(const Expr& a, T&& b) {
  return a + to_expr_cast(std::forward<T>(b));
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator-(const Expr& a, T&& b) {
  return a - to_expr_cast(std::forward<T>(b));
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator*(const Expr& a, T&& b) {
  return a * to_expr_cast(std::forward<T>(b));
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator/(const Expr& a, T&& b) {
  return a / to_expr_cast(std::forward<T>(b));
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator+(T&& a, const Expr& b) {
  return to_expr_cast(std::forward<T>(a)) + b;
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator-(T&& a, const Expr& b) {
  return to_expr_cast(std::forward<T>(a)) - b;
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator*(T&& a, const Expr& b) {
  return to_expr_cast(std::forward<T>(a)) * b;
}
template <typename T>
std::enable_if_t<IsCastableToExpr<T>, Expr> operator/(T&& a, const Expr& b) {
  return to_expr_cast(std::forward<T>(a)) / b;
}

// Custom literal suffix support.
inline Expr operator"" _s(unsigned long long int arg) {
  return Expr::FromInt(static_cast<int64_t>(arg));
}
inline Expr operator"" _s(long double arg) { return Expr::FromFloat(static_cast<double>(arg)); }

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{std::make_shared<const T>(std::forward<Args>(args)...)};
}

}  // namespace math
