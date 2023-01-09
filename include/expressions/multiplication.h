// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>
#include <array>

#include "constants.h"
#include "operation_bases.h"

namespace math {

// A multiplication of `N` terms.
class Multiplication : public NAryOp<Multiplication> {
 public:
  static constexpr const char* NameStr = "Multiplication";

  // Do not call this - use `FromTwoOperands`.
  explicit Multiplication(std::vector<Expr> args);

  // Split a multiplication into numerator and denominator terms, based on the sign of the
  // exponents. Terms on the denominator are re-written to have positive exponents.
  std::pair<Expr, Expr> SplitByExponent() const;

  // Construct from two operands. Creates an expression corresponding to: a * b
  static Expr FromTwoOperands(const Expr& a, const Expr& b) {
    // TODO: Dumb that we allocate for this, but in future it will be an inline vector:
    return Multiplication::FromOperands({a, b});
  }

  // Construct form a vector of operands. The result is automatically simplified, and may not
  // be a multiplication.
  static Expr FromOperands(const std::vector<Expr>& args);

  // Convert the vector of arguments to canonical form (modified in place).
  static Expr CanonicalizeArguments(std::vector<Expr>& args);
};

// Convert an expression into a coefficient and a multiplicand. This operation checks if
// expr is a multiplication. If it is, we extract all numeric constants and return them
// as the first value. The remaining terms form a new multiplication, which is returned as
// the second value.
std::pair<Expr, Expr> AsCoefficientAndMultiplicand(const Expr& expr);

}  // namespace math
