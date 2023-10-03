// Copyright 2022 Gareth Cross
#include "expressions/numeric_expressions.h"

#include "constants.h"

namespace math {

// Defined here to resolve a circular import issue for now.
Expr Integer::create(IntegralType x) {
  if (x == 0) {
    return Constants::Zero;
  } else if (x == 1) {
    return Constants::One;
  } else if (x == -1) {
    return Constants::NegativeOne;
  }
  return make_expr<Integer>(x);
}

}  // namespace math
