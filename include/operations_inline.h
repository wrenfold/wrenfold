#pragma once
#include <type_traits>

#include "assertions.h"
#include "binary_operations.h"
#include "constant_expressions.h"
#include "constants.h"
#include "expression.h"
#include "unary_operations.h"
#include "visitor_impl.h"

namespace math {

// TODO: Delete division.
inline Expr Div(const Expr& a, const Expr& b) {
  if (IsZero(a)) {
    // TODO(gareth): Throw when b is zero.
    return Constants::Zero;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExpr<Division>(a, b);
}

inline Expr operator*(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, b);
}

inline Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

inline Expr operator-(const Expr& a, const Expr& b) { return a + Negation::Create(b); }

inline Expr operator/(const Expr& a, const Expr& b) { return Div(a, b); }

}  // namespace math
