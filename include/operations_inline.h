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

inline Expr Negate(const Expr& x) {
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

inline Expr Pow(const Expr& a, const Expr& b) {
  ASSERT(!IsZero(a) || !IsZero(b), "TODO: Implement proper handling of 0^0");
  if (IsZero(a)) {
    return Constants::Zero;
  }
  if (IsZero(b)) {
    return Constants::One;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExpr<Power>(a, b);
}

inline Expr Log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  return MakeExpr<NaturalLog>(x);
}

inline Expr operator*(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, b);
}

inline Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

inline Expr operator-(const Expr& a, const Expr& b) { return a + Negate(b); }

inline Expr operator/(const Expr& a, const Expr& b) { return Div(a, b); }

}  // namespace math
