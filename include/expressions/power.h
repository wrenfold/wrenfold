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

}  // namespace math
