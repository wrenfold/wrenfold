#pragma once
#include "expression_concept.h"
#include "operation_base.h"

namespace math {

template <typename Derived>
class UnaryOp : public ExpressionImpl<Derived> {
 public:
  explicit UnaryOp(const Expr& x) : x_(x) {}
  explicit UnaryOp(Expr&& x) : x_(std::move(x)) {}

  // Test unary ops for equality.
  bool IsIdenticalToImplTyped(const UnaryOp<Derived>& neg) const {
    return x_.GetImpl()->IsIdenticalTo(neg.x_.GetImpl());
  }

  // Get inner expression.
  const Expr& Inner() const { return x_; }

 protected:
  Expr x_;
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
