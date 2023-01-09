// Copyright 2022 Gareth Cross
#pragma once
#include "constants.h"
#include "operation_bases.h"
#include "visitor_impl.h"

namespace math {

class Addition : public NAryOp<Addition> {
 public:
  static constexpr const char* NameStr = "Addition";

  // Do not call this - use `FromTwoOperands`.
  explicit Addition(std::vector<Expr> args);

  // Construct from two operands.
  static Expr FromTwoOperands(const Expr& a, const Expr& b) { return FromOperands({a, b}); }

  // Construct form a vector of operands.
  // The result is automatically simplified, and may not be an addition.
  static Expr FromOperands(const std::vector<Expr>& args);
};

}  // namespace math
