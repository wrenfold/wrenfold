#pragma once
#include "expression_concept.h"

namespace math {

template <typename Derived>
class UnaryOp : public ExpressionImpl<Derived> {
 public:
  explicit UnaryOp(const Expr& x) : x_(x) {}
  explicit UnaryOp(Expr&& x) : x_(std::move(x)) {}

  // Test unary ops for equality.
  bool IsIdenticalToImplTyped(const UnaryOp<Derived>& neg) const {
    return x_.IsIdenticalTo(neg.x_);
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

  static Expr Create(const Expr& x) {
    if (IsZero(x)) {
      return Constants::Zero;
    }
    // If this is already negated, flip it back:
    if (const std::optional<Expr> inner =
            TryVisit(x, [](const Negation& neg) { return neg.Inner(); });
        inner) {
      return inner.value();
    }
    return MakeExpr<Negation>(x);
  }
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

  static Expr Create(const Expr& x) {
    if (x.IsIdenticalTo(Constants::Euler)) {
      return Constants::One;
    }
    return MakeExpr<NaturalLog>(x);
  }
};

}  // namespace math
