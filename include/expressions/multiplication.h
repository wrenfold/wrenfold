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
  static constexpr std::string_view NameStr = "Multiplication";

  // Do not call this - use `FromTwoOperands`.
  explicit Multiplication(std::vector<Expr> args);

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

// A decomposition of `Multiplication` that is more convenient for printing.
// This is stored in and not in one particular formatter, since it is likely useful more than once.
struct MultiplicationFormattingInfo {
  struct BaseExp {
    Expr base;
    Expr exponent;
  };

  bool is_negative{false};
  std::vector<std::variant<Integer, Float, BaseExp>> numerator;
  std::vector<std::variant<Integer, Float, BaseExp>> denominator;
};

// Create `MultiplicationFormattingInfo` from a multiplication.
MultiplicationFormattingInfo GetFormattingInfo(const Multiplication& mul);

}  // namespace math
