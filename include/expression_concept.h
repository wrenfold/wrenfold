#pragma once
#include "expression_fwd.h"

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

  // Apply a visitor to this expression.
  virtual void Receive(class VisitorBase& visitor) const = 0;

  // Get the string name of the underlying expression.
  virtual std::string_view TypeName() const = 0;

  // Is this expression a leaf-node type.
  virtual bool IsLeaf() const = 0;

 protected:
  // Implemented by derived class. Called after we check ptr address.
  virtual bool IsIdenticalToImpl(const ExpressionConcept& other) const = 0;
};

}  // namespace math
