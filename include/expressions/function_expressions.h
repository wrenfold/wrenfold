#pragma once
#include "expression_concept.h"
#include "expression_impl.h"
#include "expressions/numeric_expressions.h"
#include "operation_bases.h"

// Expressions for common mathematical functions.
namespace math {

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
