#pragma once
#include "visitor_base.h"

namespace math {

// Base class for all expressions.
class ExpressionConcept {
 public:
  virtual ~ExpressionConcept() = default;

  // Test if two expressions are identical.
  virtual bool IsIdenticalTo(const ExpressionConcept& other) const = 0;

  // Apply a visitor to this expression.
  virtual void Receive(VisitorBase& visitor) const = 0;

  // Get the string name of the underlying expression.
  virtual std::string_view TypeName() const = 0;

  // Is this expression a leaf-node type.
  virtual bool IsLeaf() const = 0;

  // Check if the underlying derived type is one of `... Ts`.
  template <typename... Ts>
  bool IsType() const {
    static_assert((ContainsType<Ts, ExpressionTypeList> && ...),
                  "Ts is not a valid expression type");
    return (TypeMatchesIndex(IndexOfType<Ts, ExpressionTypeList>::Value) || ...);
  }

 protected:
  // True if the underlying type matches the provided index.
  virtual bool TypeMatchesIndex(std::size_t index) const = 0;
};

}  // namespace math
