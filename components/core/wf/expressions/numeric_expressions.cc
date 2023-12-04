// Copyright 2022 Gareth Cross
#include "wf/expressions/numeric_expressions.h"

#include "wf/constants.h"

namespace math {

// Defined here to resolve a circular import issue for now.
Expr Integer::create(IntegralType x) {
  if (x == 0) {
    return constants::zero;
  } else if (x == 1) {
    return constants::one;
  } else if (x == -1) {
    return constants::negative_one;
  }
  return make_expr<Integer>(x);
}

}  // namespace math
