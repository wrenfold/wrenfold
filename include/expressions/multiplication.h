// Copyright 2022 Gareth Cross
#pragma once
#include <array>
#include <unordered_map>
#include <variant>

#include "constants.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"
#include "operation_bases.h"

namespace math {

// A multiplication of `N` terms.
class Multiplication : public NAryOp<Multiplication> {
 public:
  static constexpr std::string_view NameStr = "Multiplication";
  static constexpr bool IsLeafNode = false;

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

// Helper object used to execute multiplications.
struct MultiplicationParts {
  MultiplicationParts() = default;
  explicit MultiplicationParts(std::size_t capacity) { terms.reserve(capacity); }

  // Construct from existing multiplication.
  explicit MultiplicationParts(const Multiplication& mul, bool factorize_integers);

  // Rational coefficient.
  Rational rational_coeff{1, 1};
  // Floating point coefficient:
  std::optional<Float> float_coeff{};
  // Map from base to exponent.
  std::unordered_map<Expr, Expr, ExprHash, ExprEquality> terms{};

  // Update the internal product by multiplying on `arg`.
  void Multiply(const Expr& arg, bool factorize_integers = false);

  // Nuke any terms w/ a zero exponent and normalize powers of integers.
  void Normalize();

  // Create the resulting multiplication. The provided storage is re-used.
  Expr CreateMultiplication(std::vector<Expr>&& args) const;
};

// A decomposition of `Multiplication` that is more convenient for printing.
// This is defined here and not in one particular formatter, since it is likely useful more than
// once.
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
