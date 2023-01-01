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
  explicit Expr(const std::string& name);

  // Construct constant from integer.
  Expr(double x);

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

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{std::make_shared<const T>(std::forward<Args>(args)...)};
}

}  // namespace math
