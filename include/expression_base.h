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

  // Convert to string.
  virtual std::string ToString() const = 0;

  // Cast to particular type.
  template <typename T>
  const T* As() const {
    return dynamic_cast<const T*>(this);
  }

  // Test for equality.
  bool Equals(const ExpressionBase& other) const {
    if (this == &other) {
      // Do simple pointer equality check first.
      return true;
    }
    return EqualsImpl(other);
  }

  // Variant of `Equals` that accepts pointer.
  bool Equals(const ExpressionBaseConstPtr& other_ptr) const { return Equals(*other_ptr); }

 protected:
  virtual bool EqualsImpl(const ExpressionBase& other) const = 0;
};

// CRTP child of ExpressionBase. Other children inherit form this.
template <typename Derived>
class ExpressionImpl : public ExpressionBase {
 public:
  ~ExpressionImpl() override = default;

 protected:
  bool EqualsImpl(const ExpressionBase& other) const override {
    const Derived* const typed_other = other.As<Derived>();
    return typed_other && static_cast<const Derived*>(this)->EqualsImplTyped(*typed_other);
  }
};

// Make a shared pointer to an expression. Return as base-ptr type.
template <typename T, typename... Args>
ExpressionBaseConstPtr MakeExprBase(Args&&... args) {
  return std::make_shared<const T>(std::forward<Args>(args)...);
}

}  // namespace math

