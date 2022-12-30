#pragma once
#include "constant_expressions.h"
#include "expression_concept.h"
#include "expression_impl.h"
#include "operation_types.h"

namespace math {

// Negation an expression: -x
class Negation : public UnaryOp<Negation> {
 public:
  using UnaryOp::UnaryOp;

  static Expr Create(const Expr& x) {
    if (IsZero(x)) {
      return Constants::Zero;
    }
    // -(-x) -> x
    // If this is already negated, flip it back:
    if (const std::optional<Expr> inner =
            VisitLambda(x, [](const Negation& neg) { return neg.Inner(); });
        inner) {
      return inner.value();
    }
    // If this is a number, just flip the sign.
    if (const std::optional<Expr> negated_int =
            VisitLambda(x, [](const Integer& num) { return MakeExpr<Integer>(-num.GetValue()); });
        negated_int) {
      return *negated_int;
    }
    if (const std::optional<Expr> negated_float =
            VisitLambda(x, [](const Float& num) { return MakeExpr<Float>(-num.GetValue()); });
        negated_float) {
      return *negated_float;
    }
    return MakeExpr<Negation>(x);
  }
};

// Take natural log: ln(x)
class NaturalLog : public UnaryOp<NaturalLog> {
 public:
  using UnaryOp::UnaryOp;

  static Expr Create(const Expr& x) {
    if (x.IsIdenticalTo(Constants::Euler)) {
      return Constants::One;
    }
    return MakeExpr<NaturalLog>(x);
  }
};

}  // namespace math
