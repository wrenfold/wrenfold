// Copyright 2022 Gareth Cross
#pragma once

#include "common_visitors.h"
#include "expression.h"
#include "operation_bases.h"
#include "visitor_impl.h"

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
inline std::pair<Expr, Expr> AsBaseAndExponent(const Expr& expr) {
  const auto result = VisitLambda(expr, [](const Power& power) {
    // Return as base/exponent pair.
    return std::make_pair(power.Base(), power.Exponent());
  });
  if (result.has_value()) {
    return *result;
  }
  return std::make_pair(expr, Constants::One);
}

// Convert an
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
