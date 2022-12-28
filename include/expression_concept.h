#pragma once
#include "expression_fwd.h"
#include "visitor_base.h"

namespace math {

// Base class for all expressions.
class ExpressionConcept {
 public:
  virtual ~ExpressionConcept() = default;

  // Cast to particular type.
  template <typename T>
  const T* As() const {
    return dynamic_cast<const T*>(this);
  }

  // Test if two expressions are identical.
  bool IsIdenticalTo(const ExpressionConcept& other) const { return IsIdenticalToImpl(other); }

  // Variant of `IsIdenticalTo` that accepts shared pointer.
  bool IsIdenticalTo(const ExpressionConceptConstPtr& other_ptr) const {
    return IsIdenticalTo(*other_ptr);
  }

  // Apply a visitor that returns an expression pointer.
  virtual ExpressionConceptConstPtr Receive(VisitorWithResultBase& visitor) const = 0;

  // Apply a visitor that does not return anything.
  virtual void Receive(VisitorWithoutResultBase& visitor) const = 0;

 protected:
  // Implemented by derived class. Called after we check ptr address.
  virtual bool IsIdenticalToImpl(const ExpressionConcept& other) const = 0;
};

// CRTP child of ExpressionConcept. Other children inherit form this.
template <typename Derived>
class ExpressionImpl : public ExpressionConcept {
 public:
  ~ExpressionImpl() override = default;

  // Cast to the derived type.
  const Derived& AsDerived() const { return static_cast<const Derived&>(*this); }

  // Cast to derived type and apply the visitor.
  ExpressionConceptConstPtr Receive(VisitorWithResultBase& visitor) const override {
    return visitor.ApplyVirtual(AsDerived());
  }

  void Receive(VisitorWithoutResultBase& visitor) const override {
    visitor.ApplyVirtual(AsDerived());
  }

 protected:
  bool IsIdenticalToImpl(const ExpressionConcept& other) const override {
    const Derived* const typed_other = other.As<Derived>();
    return typed_other && static_cast<const Derived*>(this)->IsIdenticalToImplTyped(*typed_other);
  }
};

}  // namespace math
