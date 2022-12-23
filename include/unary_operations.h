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

// Negation an expression: -x
class Negation : public UnaryOp<Negation> {
 public:
  using UnaryOp::UnaryOp;
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
};

}  // namespace math
