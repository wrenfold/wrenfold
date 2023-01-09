// Copyright 2022 Gareth Cross
#pragma once

#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "operation_bases.h"

namespace math {

// Power operation: base^exponent
class Power : public BinaryOp<Power> {
 public:
  using BinaryOp::BinaryOp;

  // Create a new power.
  // Will apply rules to simplify automatically.
  static Expr Create(const Expr& a, const Expr& b);

  const Expr& Base() const { return first_; }
  const Expr& Exponent() const { return second_; }
};

// Convert an expression to a base/exponent pair.
std::pair<Expr, Expr> AsBaseAndExponent(const Expr& expr);

// Convert a rational exponent to the whole integer part and the remainder.
// If the exponent is negative, we add to the whole integer part so that the rational part
// can be positive (i.e. we eliminate "absurd" rationals).
inline std::pair<Integer, Rational> FactorizeRationalExponent(const Rational& r) {
  const Integer integer_part{r.Numerator() / r.Denominator()};
  const Rational fractional_part_signed{r.Numerator() % r.Denominator(), r.Denominator()};
  if (r.Numerator() >= 0) {
    return std::make_pair(integer_part, fractional_part_signed);
  } else {
    // If negative, we subtract one from the integer part and make the rational part positive:
    return std::make_pair(Integer{integer_part.GetValue() - 1},
                          fractional_part_signed + Rational{1, 1});
  }
}

}  // namespace math
