#pragma once
#include "expression_base.h"

namespace math {

template <typename Derived>
class UnaryOp : public ExpressionImpl<Derived> {
 public:
  explicit UnaryOp(const ExpressionBaseConstPtr& x) : x_(x) {}
  explicit UnaryOp(ExpressionBaseConstPtr&& x) : x_(std::move(x)) {}

  // Test unary ops for equality.
  bool IsIdenticalToImplTyped(const UnaryOp<Derived>& neg) const {
    return x_->IsIdenticalTo(neg.x_);
  }

  // Get inner expression.
  const ExpressionBaseConstPtr& Inner() const { return x_; }

 protected:
  ExpressionBaseConstPtr x_;
};

// Negate an expression.
class Negate : public UnaryOp<Negate> {
 public:
  using UnaryOp::UnaryOp;

  // Returns the derivative of the inner object, negated.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;
};

class NaturalLog : public UnaryOp<NaturalLog> {
 public:
  using UnaryOp::UnaryOp;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;
};

}  // namespace math
