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

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T>
  Expr(T v, std::enable_if_t<std::is_integral_v<T> || std::is_floating_point_v<T>, void*> = nullptr)
      : Expr(ConstructImplicit(v)) {}

  // Get the implementation pointer.
  const ExpressionConceptConstPtr& GetImpl() const { return impl_; }

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
  void Receive(VisitorBase& visitor) const { impl_->Receive(visitor); }

 private:
  // Construct constant from float.
  static Expr FromFloat(double x);

  // Construct from integer.
  static Expr FromInt(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr ConstructImplicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    if constexpr (std::is_integral_v<T>) {
      return FromInt(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return FromFloat(static_cast<double>(v));
    }
  }

  ExpressionConceptConstPtr impl_;
};

static_assert(std::is_move_assignable_v<Expr> && std::is_move_constructible_v<Expr>,
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

// Custom literal suffix support.
namespace custom_literals {
inline Expr operator"" _s(unsigned long long int arg) { return Expr{arg}; }
inline Expr operator"" _s(long double arg) { return Expr{arg}; }
}  // namespace custom_literals

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{std::make_shared<const T>(std::forward<Args>(args)...)};
}

}  // namespace math
