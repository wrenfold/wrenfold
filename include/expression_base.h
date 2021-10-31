#pragma once
#include "expr.h"

namespace math {
class Variable;

// Base class for all expressions.
class ExpressionBase {
 public:
  virtual ~ExpressionBase() = default;

  // Differentiate.
  virtual ExpressionBaseConstPtr Diff(const Variable& var) const = 0;

  // Cast to particular type.
  template <typename T>
  const T* As() const {
    return dynamic_cast<const T*>(this);
  }

  // Test for equality.
  bool IsIdenticalTo(const ExpressionBase& other) const {
    if (this == &other) {
      // Do simple pointer equality check first.
      return true;
    }
    return IsIdenticalToImpl(other);
  }

  // Variant of `IsIdenticalTo` that accepts shared pointer.
  bool IsIdenticalTo(const ExpressionBaseConstPtr& other_ptr) const {
    return IsIdenticalTo(*other_ptr);
  }

  // Convert to string.
  virtual std::string Format() const = 0;

 protected:
  // Implemented by derived class. Called after we check ptr address.
  virtual bool IsIdenticalToImpl(const ExpressionBase& other) const = 0;
};

// CRTP child of ExpressionBase. Other children inherit form this.
template <typename Derived>
class ExpressionImpl : public ExpressionBase {
 public:
  ~ExpressionImpl() override = default;

 protected:
  bool IsIdenticalToImpl(const ExpressionBase& other) const override {
    const Derived* const typed_other = other.As<Derived>();
    return typed_other && static_cast<const Derived*>(this)->IsIdenticalToImplTyped(*typed_other);
  }
};

// Make a shared pointer to an expression. Return as base-ptr type.
template <typename T, typename... Args>
ExpressionBaseConstPtr MakeExprBase(Args&&... args) {
  return std::make_shared<const T>(std::forward<Args>(args)...);
}

// Version that returns Expr.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{MakeExprBase<T>(std::forward<Args>(args)...)};
}

}  // namespace math
