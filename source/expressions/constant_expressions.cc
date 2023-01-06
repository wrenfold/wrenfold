// Copyright 2022 Gareth Cross
#include "expressions/numeric_expressions.h"

#include "constants.h"

namespace math {

Expr Integer::Create(IntegralType x) {
  if (x == 0) {
    return Constants::Zero;
  } else if (x == 1) {
    return Constants::One;
  } else if (x == -1) {
    return Constants::NegativeOne;
  }
  return MakeExpr<Integer>(x);
}

Expr Rational::Create(Rational r) {
  // Make sure this isn't actually an integer:
  if (auto as_int = r.TryConvertToInteger(); as_int) {
    return Integer::Create(as_int->GetValue());
  }
  return MakeExpr<Rational>(r);
}

}  // namespace math
