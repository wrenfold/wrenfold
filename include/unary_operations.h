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

// Negate an expression: -x
class Negate : public UnaryOp<Negate> {
 public:
  using UnaryOp::UnaryOp;

  // Returns the derivative of the inner object, negated.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;
};

// Base class for unary functions.
template <typename Derived>
class UnaryFunction : public UnaryOp<Derived> {
 public:
  using UnaryOp<Derived>::UnaryOp;
};

// Take natural log: ln(x)
class NaturalLog : public UnaryFunction<NaturalLog> {
 public:
  using UnaryFunction::UnaryFunction;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;
};

}  // namespace math
