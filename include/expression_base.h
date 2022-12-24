#pragma once
#include "expression_fwd.h"
#include "formatting.h"
#include "visitor.h"

namespace math {

// Base class for all expressions.
class ExpressionBase {
 public:
  virtual ~ExpressionBase() = default;

  // Cast to particular type.
  template <typename T>
  const T* As() const {
    return dynamic_cast<const T*>(this);
  }

  // Test if two expressions are identical.
  bool IsIdenticalTo(const ExpressionBase& other) const { return IsIdenticalToImpl(other); }

  // Variant of `IsIdenticalTo` that accepts shared pointer.
  bool IsIdenticalTo(const ExpressionBaseConstPtr& other_ptr) const {
    return IsIdenticalTo(*other_ptr);
  }

  // Use the provided formatter to format an expression, appending to `output`.
  virtual void Format(const Formatter& formatter, std::string& output) const = 0;

  // Apply a visitor that returns an expression pointer.
  virtual ExpressionBaseConstPtr Receive(VisitorWithResultBase& visitor) const = 0;

 protected:
  // Implemented by derived class. Called after we check ptr address.
  virtual bool IsIdenticalToImpl(const ExpressionBase& other) const = 0;
};

// CRTP child of ExpressionBase. Other children inherit form this.
template <typename Derived>
class ExpressionImpl : public ExpressionBase {
 public:
  ~ExpressionImpl() override = default;

  // Cast to the derived type.
  const Derived& AsDerived() const { return static_cast<const Derived&>(*this); }

  // Format the expression. Calls the formatter on our derived type.
  void Format(const Formatter& formatter, std::string& output) const override {
    formatter.Format(AsDerived(), output);
  }

  // Cast to derived type and apply the visitor.
  ExpressionBaseConstPtr Receive(VisitorWithResultBase& visitor) const override {
    return visitor.ApplyVirtual(AsDerived());
  }

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

}  // namespace math
