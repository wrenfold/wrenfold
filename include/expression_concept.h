#pragma once
#include "expression_fwd.h"

namespace math {

template <typename T>
class VisitorBase;

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
  virtual Expr Receive(VisitorBase<Expr>& visitor) const = 0;

  // Apply a visitor that does not return anything.
  virtual void Receive(VisitorBase<void>& visitor) const = 0;

 protected:
  // Implemented by derived class. Called after we check ptr address.
  virtual bool IsIdenticalToImpl(const ExpressionConcept& other) const = 0;
};

}  // namespace math
