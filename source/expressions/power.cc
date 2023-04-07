// Copyright 2022 Gareth Cross
#include "expressions/power.h"

#include <algorithm>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "integer_utils.h"
#include "visitor_impl.h"

namespace math {

struct PowerNumerics {
  using ReturnType = Expr;
  using Policy = VisitorPolicy::NoError;

  // If either operand is a float, coerce the other to float:
  template <typename A, typename B>
  std::enable_if_t<IsFloatAndNumeric<A, B>, Expr> Apply(const A& a, const B& b) {
    const auto result =
        std::pow(static_cast<Float>(a).GetValue(), static_cast<Float>(b).GetValue());
    return MakeExpr<Float>(result);
  }

  // If both operands are integers:
  Expr Apply(const Integer& a, const Integer& b) {
    if (b.GetValue() < 0) {
      ASSERT_NOT_EQUAL(a.GetValue(), 0, "TODO: Handle taking 0 to a negative power?");
      // Convert a -> (1/a), then take the power:
      return Apply(Rational{1, a.GetValue()}, -b);
    }
    // For everything else, resort to calling Pow(...), b is > 0 here:
    const auto pow = Pow(a.GetValue(), b.GetValue());
    return Integer::Create(pow);
  }

  // If the left operand is a rational and right operand is integer:
  Expr Apply(const Rational& a, const Integer& b) {
    const auto exponent = b.GetValue();
    const auto n = Pow(a.Numerator(), std::abs(exponent));
    const auto d = Pow(a.Denominator(), std::abs(exponent));
    if (exponent >= 0) {
      return Rational::Create(n, d);
    } else {
      // Flip the rational:
      return Rational::Create(d, n);
    }
  }

  // If the left operand is integer, and the right is rational:
  Expr Apply(const Integer& a, const Rational& b) {
    ASSERT_GREATER(b.Denominator(), 0, "Rational must have positive denominator");

    // Factorize the integer into primes:
    const std::vector<PrimeFactor> factors = ComputePrimeFactors(a.GetValue());
    ASSERT(std::is_sorted(factors.begin(), factors.end(),
                          [](const auto& x, const auto& y) { return x.base < y.base; }),
           "Factors should be sorted");

    // Next we will create expressions.
    std::vector<Expr> operands{};
    operands.reserve(factors.size() + 1);

    // Iterate over factors and put them into canonical form:
    // negative example:
    //    2 ^ (-3/7) --> (1/2) * 2 ^ (4/7)
    //    2 ^ (-12/7) --> (1/2) * 2 ^ (-5/7) --> (1/4) * 2 ^ (2/7)
    // positive example:
    //    2 ^ (18/7) --> 4 * 2 ^ (4/7)
    // See https://arxiv.org/pdf/1302.2169.pdf for examples of canonical forms.
    Rational rational_coeff{1, 1};
    for (const PrimeFactor& f : factors) {
      // Multiply the power by the rational to get the exponent applied to this prime factor:
      const Rational actual_exp = b * static_cast<Rational>(Integer{f.exponent});
      ASSERT_GREATER(f.exponent, 0);  //  Exponents must be >= 1 in this context.

      // Factorize the exponent: x^(int_part + frac_part) --> x^int_part * x^frac_part
      // Both of these should have the same sign as actual_exp.
      const auto [integer_part, fractional_part] = FactorizeRationalExponent(actual_exp);

      // Apply the integer part to the rational coefficient:
      if (integer_part.GetValue() >= 0) {
        rational_coeff = rational_coeff * Rational{Pow(f.base, integer_part.GetValue()), 1};
      } else {
        rational_coeff = rational_coeff * Rational{1, Pow(f.base, -integer_part.GetValue())};
      }

      // There is still the business of the fractional part to deal with:
      if (fractional_part.Numerator() != 0) {
        Expr base = Integer::Create(f.base);
        Expr exponent = Rational::Create(fractional_part);
        operands.push_back(MakeExpr<Power>(std::move(base), std::move(exponent)));
      }
    }

    if (!rational_coeff.IsOne()) {
      operands.push_back(Rational::Create(rational_coeff));
    }
    if (operands.size() == 1) {
      return operands.front();
    }
    return Multiplication::FromOperands(operands);
  }
};

Expr Power::Create(const Expr& a, const Expr& b) {
  // Check for numeric quantities.
  std::optional<Expr> numeric_pow = VisitBinaryStruct(a, b, PowerNumerics{});
  if (numeric_pow) {
    return *numeric_pow;
  }

  // Check if the base is itself a power:
  if (const Power* a_pow = CastPtr<Power>(a); a_pow != nullptr) {
    // We want to avoid doing incorrect simplifications like:
    //    (x^2)^(1/2) --> x (incorrect, loses the sign of x)
    //    (x^2)^0.5 --> x
    //    (x^2)^z --> x^(2*z) (incorrect, z could be 1/2 or 0.5)
    //    (x^y)^(1/4) --> x^(y/4) (incorrect, y could be 4)
    // If the inner is already a rational, or the outer is an integer - we can multiply
    // safely. If the outer is a float, it can be multiplied onto rationals or floats.
    const bool can_multiply_exponents = a_pow->Exponent().Is<Rational>() || b.Is<Integer>() ||
                                        (a_pow->Exponent().Is<Float>() && b.Is<Float>());
    if (can_multiply_exponents) {
      return Power::Create(a_pow->Base(), a_pow->Exponent() * b);
    }
  }

  // Check for zeroes:
  if (IsZero(a)) {
    // 0^x -> 0  (TODO: Only true for real x)
    return Constants::Zero;
  } else if (IsZero(b)) {
    // x^0 -> 1
    return Constants::One;
  } else if (IsOne(b)) {
    // x^1 -> x
    return a;
  }

  // Check if the base is a multiplication.
  // In this case, we convert to a multiplication of powers:
  if (const Multiplication* const mul = CastPtr<Multiplication>(a); mul != nullptr) {
    std::vector<Expr> args;
    args.reserve(mul->Arity());
    for (const Expr& arg : mul->Args()) {
      args.push_back(Power::Create(arg, b));
    }
    return Multiplication::FromOperands(args);
  }
  return MakeExpr<Power>(a, b);
}

std::pair<Expr, Expr> AsBaseAndExponent(const Expr& expr) {
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
