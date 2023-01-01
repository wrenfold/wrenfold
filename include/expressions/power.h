// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>

#include "common_visitors.h"
#include "expression.h"
#include "operation_bases.h"
#include "visitor_impl.h"

namespace math {

// Power operation: base^exponent
class Power : public BinaryOp<Power> {
 public:
  using BinaryOp::BinaryOp;

  static Expr Create(const Expr& a, const Expr& b) {
    // Check for zeroes:
    if (IsZero(a) && IsZero(b)) {
      // 0^0 -> 1
      return Constants::One;
    } else if (IsZero(a)) {
      // 0^x -> 0  (TODO: Only true for real x)
      return Constants::Zero;
    } else if (IsZero(b)) {
      // x^0 -> 1
      return Constants::One;
    }
    // Check for an exponent that is identically one:
    if (IsOne(b)) {
      // x^1 -> x
      return a;
    } else if (IsOne(a) && IsIntegralValue(b)) {
      // 1^n -> 1 (where n is integral constant)
      return Constants::One;
    }
    return MakeExpr<Power>(a, b);
  }

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

}  // namespace math
