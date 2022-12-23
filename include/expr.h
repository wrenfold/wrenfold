#pragma once
#include <memory>
#include <string>

#include "expression_base.h"
#include "operation_utils.h"

namespace math {

/**
 * Wrapper around a pointer to an abstract expression. Defined so you can easily write chains of
 * operations without dealing with pointers at all.
 */
class Expr {
 public:
  // Constructors.
  explicit Expr(ExpressionBaseConstPtr&& impl) : impl_(std::move(impl)) {}
  explicit Expr(const ExpressionBaseConstPtr& impl) : impl_(impl) {}

  // Construct variable:
  explicit Expr(const std::string& name);

  // Construct constant. Implicit so we can use numbers in math operations.
  Expr(double x);

  // Get the implementation pointer.
  const ExpressionBaseConstPtr& GetImpl() const { return impl_; }

  // Get a raw pointer, cast dynamically to a particular type.
  template <typename T>
  const T* GetRaw() const {
    return dynamic_cast<const T*>(impl_.get());
  }

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const Expr& other) const { return impl_->IsIdenticalTo(other.GetImpl()); }

  // Convert to string.
  std::string ToString() const;

  // Negation operator.
  Expr operator-() const { return Expr{Negate(impl_)}; }

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int Reps = 1) const;

 private:
  ExpressionBaseConstPtr impl_;
};

// Operations defined on expressions:
inline Expr operator*(const Expr& a, const Expr& b) { return Expr{Mul(a.GetImpl(), b.GetImpl())}; }
inline Expr operator+(const Expr& a, const Expr& b) { return Expr{Add(a.GetImpl(), b.GetImpl())}; }
inline Expr operator-(const Expr& a, const Expr& b) { return Expr{Sub(a.GetImpl(), b.GetImpl())}; }
inline Expr operator/(const Expr& a, const Expr& b) { return Expr{Div(a.GetImpl(), b.GetImpl())}; }
inline Expr operator^(const Expr& a, const Expr& b) { return Expr{Pow(a.GetImpl(), b.GetImpl())}; }

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{MakeExprBase<T>(std::forward<Args>(args)...)};
}

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToString();
  return stream;
}

}  // namespace math
