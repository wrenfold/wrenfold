// Copyright 2022 Gareth Cross
#pragma once
#include <algorithm>
#include <array>

#include "common_visitors.h"
#include "constants.h"
#include "operation_bases.h"
#include "visitor_impl.h"

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
  static void CanonicalizeArguments(std::vector<Expr>& args);
};

struct CoefficientVisitor {
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  enum class Result {
    Coefficient,
    MulWithCoefficient,
    Neither,
  };
  using ReturnType = Result;

  ReturnType Apply(const Multiplication& mul) const {
    if (IsNumeric(mul[0])) {
      return Result::MulWithCoefficient;
    }
    return Result::Neither;
  }

  constexpr ReturnType Apply(const Integer&) const { return Result::Coefficient; }
  constexpr ReturnType Apply(const Float&) const { return Result::Coefficient; }

  // Visit an expression and determine what form it is in.
  static Result Visit(const Expr& expr) {
    return VisitStruct(expr, CoefficientVisitor{}).value_or(Result::Neither);
  }

  // Visit the expression and extract the coefficient.
  static Expr GetCoefficient(const Expr& expr) {
    // Determine the format of this expression:
    const Result type = Visit(expr);
    // Pull out the coefficient:
    switch (type) {
      case CoefficientVisitor::Result::Coefficient:
        // The expression is a coefficient, so return (c, 1)
        return expr;
      case CoefficientVisitor::Result::MulWithCoefficient: {
        // The annoying case - the expression is a multiplication where the first term is
        // a coefficient:
        const Multiplication& mul = *expr.StaticCast<Multiplication>();
        return mul[0];
      }
      case CoefficientVisitor::Result::Neither:
        // All other cases, just fall through and return (1, expr)
        break;
    }
    return Constants::One;
  }
};

}  // namespace math
