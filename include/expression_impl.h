#pragma once
#include "expression.h"
#include "visitor_base.h"

namespace math {

// CRTP child of ExpressionConcept. Other children inherit form this.
// TODO: This could store the underlying type via composition instead.
template <typename Derived>
class ExpressionImpl : public ExpressionConcept {
 public:
  ~ExpressionImpl() override = default;

  // Cast to the derived type.
  const Derived& AsDerived() const { return static_cast<const Derived&>(*this); }

  // Cast to derived type and apply the visitor.
  void Receive(VisitorBase& visitor) const override { visitor.ApplyVirtual(AsDerived()); }

  // Get the derived type string name (a static constexpr member).
  std::string_view TypeName() const override { return Derived::NameStr; }

  // Whether the derived type is a leaf (it contains no references to child expressions).
  static constexpr bool IsLeaf() { return Derived::IsLeafNode; }

 protected:
  // TODO: Maybe check the addresses here before trying dynamic_cast? Needs profiling.
  bool IsIdenticalToImpl(const ExpressionConcept& other) const override {
    const Derived* const typed_other = other.As<Derived>();
    return typed_other && static_cast<const Derived*>(this)->IsIdenticalToImplTyped(*typed_other);
  }
};

// Create a copy of an expression by running a unary map on its child expressions.
// The derived type should copy itself w/ new children, leaving any other properties identical.
template <typename Derived, typename Operation>
std::enable_if_t<!Derived::IsLeafNode, Expr> MapChildren(const ExpressionImpl<Derived>& expr,
                                                         Operation&& operation) {
  return expr.AsDerived().Map(std::forward<Operation>(operation));
}

}  // namespace math
