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
  Expr Receive(VisitorBase<Expr>& visitor) const override {
    return visitor.ApplyVirtual(AsDerived());
  }

  void Receive(VisitorBase<void>& visitor) const override { visitor.ApplyVirtual(AsDerived()); }

 protected:
  // TODO: Maybe check the addresses here before trying dynamic_cast? Needs profiling.
  bool IsIdenticalToImpl(const ExpressionConcept& other) const override {
    const Derived* const typed_other = other.As<Derived>();
    return typed_other && static_cast<const Derived*>(this)->IsIdenticalToImplTyped(*typed_other);
  }
};

}  // namespace math
