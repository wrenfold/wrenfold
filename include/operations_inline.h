#pragma once
#include <type_traits>

#include "assertions.h"
#include "binary_operations.h"
#include "constant_expressions.h"
#include "constants.h"
#include "expression.h"
#include "unary_operations.h"

namespace math {

inline Expr Mul(const Expr& a, const Expr& b) {
  if (IsZero(a) || IsZero(b)) {
    return Constants::Zero;
  }
  if (IsOne(a)) {
    return b;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExpr<Multiplication>(a, b);
}

inline Expr Add(const Expr& a, const Expr& b) {
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return b;
  } else if (IsZero(b)) {
    return a;
  }
  return MakeExpr<Addition>(a, b);
}

inline Expr Negate(const Expr& x) {
  if (IsZero(x)) {
    return Constants::Zero;
  }
  // If this is already negated, flip it back:
  if (const Negation* const as_negate = x.GetImpl()->As<Negation>()) {
    return as_negate->Inner();
  }
  return MakeExpr<Negation>(x);
}

inline Expr Sub(const Expr& a, const Expr& b) {
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return Negate(b);
  } else if (IsZero(b)) {
    return a;
  }
  return MakeExpr<Subtraction>(a, b);
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

inline Expr operator*(const Expr& a, const Expr& b) { return Mul(a, b); }

inline Expr operator+(const Expr& a, const Expr& b) { return Add(a, b); }

inline Expr operator-(const Expr& a, const Expr& b) { return Sub(a, b); }

inline Expr operator/(const Expr& a, const Expr& b) { return Div(a, b); }

inline Expr operator^(const Expr& a, const Expr& b) { return Pow(a, b); }

}  // namespace math
