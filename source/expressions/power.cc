// Copyright 2022 Gareth Cross
#include "expressions/power.h"

#include "expressions/multiplication.h"

namespace math {

// float -> anything: is float
// int, rational -> float: is float

// rational, int -> rational
// int, rational -> float
// rational, rational -> float
// int, int -> int

Expr Power::Create(const Expr& a, const Expr& b) {
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

  // Check if the base is a multiplication.
  // In this case, we convert to a multiplication of powers:
  if (const Multiplication* const mul = TryCast<Multiplication>(a); mul != nullptr) {
    std::vector<Expr> args;
    args.reserve(mul->Arity());
    for (const Expr& arg : mul->Args()) {
      args.push_back(Power::Create(arg, b));
    }
    return Multiplication::FromOperands(args);
  }
  return MakeExpr<Power>(a, b);
}

}  // namespace math
