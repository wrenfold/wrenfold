#pragma once
#include "assertions.h"
#include "binary_operations.h"
#include "constant_expressions.h"
#include "constants.h"
#include "unary_operations.h"

namespace math {

inline ExpressionBaseConstPtr Mul(const ExpressionBaseConstPtr& a,
                                  const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  if (IsZero(a) || IsZero(b)) {
    return Constants::Zero.GetImpl();
  }
  if (IsOne(a)) {
    return b;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExprBase<Multiplication>(a, b);
}

inline ExpressionBaseConstPtr Add(const ExpressionBaseConstPtr& a,
                                  const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return b;
  } else if (IsZero(b)) {
    return a;
  }
#ifdef VERBOSE_OPS
  fmt::print("Creating Addition({}, {})\n", ToPlainString(a), ToPlainString(b));
#endif
  return MakeExprBase<Addition>(a, b);
}

inline ExpressionBaseConstPtr Negate(const ExpressionBaseConstPtr& x) {
  ASSERT(x);
  if (IsZero(x)) {
    return Constants::Zero.GetImpl();
  }
  // If this is already negated, flip it back:
  if (const Negation* const as_negate = x->As<Negation>()) {
    return as_negate->Inner();
  }
  return MakeExprBase<Negation>(x);
}

inline ExpressionBaseConstPtr Sub(const ExpressionBaseConstPtr& a,
                                  const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  // Simplify the case where one or more operands is zero
  if (IsZero(a)) {
    return Negate(b);
  } else if (IsZero(b)) {
    return a;
  }
#ifdef VERBOSE_OPS
  fmt::print("Creating Subtraction({}, {})\n", ToPlainString(a), ToPlainString(b));
#endif
  return MakeExprBase<Subtraction>(a, b);
}

inline ExpressionBaseConstPtr Div(const ExpressionBaseConstPtr& a,
                                  const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  if (IsZero(a)) {
    // TODO(gareth): Throw when b is zero.
    return Constants::Zero.GetImpl();
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExprBase<Division>(a, b);
}

inline ExpressionBaseConstPtr Pow(const ExpressionBaseConstPtr& a,
                                  const ExpressionBaseConstPtr& b) {
  ASSERT(a);
  ASSERT(b);
  ASSERT(!IsZero(a) || !IsZero(b), "TODO: Implement proper handling of 0^0");
  if (IsZero(a)) {
    return Constants::Zero.GetImpl();
  }
  if (IsZero(b)) {
    return Constants::One.GetImpl();
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExprBase<Power>(a, b);
}

inline ExpressionBaseConstPtr Log(const ExpressionBaseConstPtr& x) {
  // TODO(gareth): Throw on invalid values...
  if (x->IsIdenticalTo(Constants::Euler.GetImpl())) {
    return Constants::One.GetImpl();
  }
  return MakeExprBase<NaturalLog>(x);
}

inline ExpressionBaseConstPtr operator*(const ExpressionBaseConstPtr& a,
                                        const ExpressionBaseConstPtr& b) {
  return Mul(a, b);
}

inline ExpressionBaseConstPtr operator+(const ExpressionBaseConstPtr& a,
                                        const ExpressionBaseConstPtr& b) {
  return Add(a, b);
}

inline ExpressionBaseConstPtr operator-(const ExpressionBaseConstPtr& a,
                                        const ExpressionBaseConstPtr& b) {
  return Sub(a, b);
}

inline ExpressionBaseConstPtr operator/(const ExpressionBaseConstPtr& a,
                                        const ExpressionBaseConstPtr& b) {
  return Div(a, b);
}

inline ExpressionBaseConstPtr operator^(const ExpressionBaseConstPtr& a,
                                        const ExpressionBaseConstPtr& b) {
  return Pow(a, b);
}

}  // namespace math
