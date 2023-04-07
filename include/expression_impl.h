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

  // Check if we can cast to this type.
  bool IsIdenticalTo(const ExpressionConcept& other) const override final {
    return other.IsType<Derived>() && static_cast<const Derived*>(this)->IsIdenticalToImplTyped(
                                          static_cast<const Derived&>(other));
  }

  // Cast to derived type and apply the visitor.
  void Receive(VisitorBase& visitor) const override { visitor.ApplyVirtual(AsDerived()); }

  // Get the derived type string name (a static constexpr member).
  std::string_view TypeName() const override { return Derived::NameStr; }

  // Whether the derived type is a leaf (it contains no references to child expressions).
  static constexpr bool IsLeafStatic() { return Derived::IsLeafNode; }

  // Virtual version of `IsLeafStatic`.
  bool IsLeaf() const override final { return IsLeafStatic(); }

 protected:
  bool TypeMatchesIndex(const std::size_t index) const override final {
    static_assert(ContainsType<Derived, ApprovedTypeList>,
                  "Derived is not a valid expression type");
    return IndexOfType<Derived, ApprovedTypeList>::Value == index;
  }
};

// Traverse all sub-expressions of the input expression. The provided `operation` will be called
// once on each child.
template <typename Derived, typename Operation>
std::enable_if_t<!Derived::IsLeafNode> IterateChildren(const ExpressionImpl<Derived>& expr,
                                                       Operation&& operation) {
  expr.AsDerived().Iterate(std::forward<Operation>(operation));
}

// Create a copy of an expression by running a unary map on its child expressions.
// The derived type should copy itself w/ new children, leaving any other properties identical.
template <typename Derived, typename Operation>
std::enable_if_t<!Derived::IsLeafNode, Expr> MapChildren(const ExpressionImpl<Derived>& expr,
                                                         Operation&& operation) {
  return expr.AsDerived().Map(std::forward<Operation>(operation));
}

}  // namespace math
