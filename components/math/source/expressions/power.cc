// Copyright 2022 Gareth Cross
#include "expressions/power.h"

#include <algorithm>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "integer_utils.h"
#include "visitor_impl.h"

namespace math {

struct PowerNumerics {
  template <typename A, typename B>
  std::optional<Expr> operator()(const A& a, const B& b) {
    if constexpr (is_float_and_numeric_v<A, B>) {
      return apply_float_and_numeric(a, b);
    } else if constexpr (std::is_same_v<Integer, A> && std::is_same_v<Integer, B>) {
      return apply_int_and_int(a, b);
    } else if constexpr (std::is_same_v<Rational, A> && std::is_same_v<Integer, B>) {
      return apply_rational_and_int(a, b);
    } else if constexpr (std::is_same_v<Integer, A> && std::is_same_v<Rational, B>) {
      return apply_int_and_rational(a, b);
    } else {
      return std::nullopt;
    }
  }

  // If either operand is a float, coerce the other to float:
  template <typename A, typename B>
  std::enable_if_t<is_float_and_numeric_v<A, B>, Expr> apply_float_and_numeric(const A& a,
                                                                               const B& b) {
    const auto result =
        std::pow(static_cast<Float>(a).get_value(), static_cast<Float>(b).get_value());
    return make_expr<Float>(result);
  }

  // If both operands are integers:
  Expr apply_int_and_int(const Integer& a, const Integer& b) {
    if (b.get_value() < 0) {
      ASSERT_NOT_EQUAL(a.get_value(), 0, "TODO: Handle taking 0 to a negative power?");
      // Convert a -> (1/a), then take the power:
      return apply_rational_and_int(Rational{1, a.get_value()}, -b);
    }
    // For everything else, resort to calling Pow(...), b is > 0 here:
    const auto pow = integer_power(a.get_value(), b.get_value());
    return Integer::create(pow);
  }

  // If the left operand is a rational and right operand is integer:
  Expr apply_rational_and_int(const Rational& a, const Integer& b) {
    const auto exponent = b.get_value();
    const auto n = integer_power(a.numerator(), std::abs(exponent));
    const auto d = integer_power(a.denominator(), std::abs(exponent));
    if (exponent >= 0) {
      return Rational::create(n, d);
    } else {
      // Flip the rational:
      return Rational::create(d, n);
    }
  }

  // If the left operand is integer, and the right is rational:
  Expr apply_int_and_rational(const Integer& a, const Rational& b) {
    ASSERT_GREATER(b.denominator(), 0, "Rational must have positive denominator");
    if (a.get_value() == 1) {
      return Constants::One;
    } else if (a.get_value() == 0) {
      return Constants::Zero;
    }

    // Factorize the integer into primes:
    const std::vector<PrimeFactor> factors = compute_prime_factors(a.get_value());
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
      const auto [integer_part, fractional_part] = factorize_rational_exponent(actual_exp);

      // Apply the integer part to the rational coefficient:
      if (integer_part.get_value() >= 0) {
        rational_coeff =
            rational_coeff * Rational{integer_power(f.base, integer_part.get_value()), 1};
      } else {
        rational_coeff =
            rational_coeff * Rational{1, integer_power(f.base, -integer_part.get_value())};
      }

      // There is still the business of the fractional part to deal with:
      if (fractional_part.numerator() != 0) {
        Expr base = Integer::create(f.base);
        Expr exponent = Rational::create(fractional_part);
        operands.push_back(make_expr<Power>(std::move(base), std::move(exponent)));
      }
    }

    if (!rational_coeff.is_one()) {
      operands.push_back(Rational::create(rational_coeff));
    }
    if (operands.size() == 1) {
      return operands.front();
    }
    return Multiplication::from_operands(operands);
  }
};

Expr Power::create(const Expr& a, const Expr& b) {
  // Check for numeric quantities.
  std::optional<Expr> numeric_pow = VisitBinary(a, b, PowerNumerics{});
  if (numeric_pow) {
    return *numeric_pow;
  }

  // Check if the base is itself a power:
  if (const Power* a_pow = cast_ptr<Power>(a); a_pow != nullptr) {
    // We want to avoid doing incorrect simplifications like:
    //    (x^2)^(1/2) --> x (incorrect, loses the sign of x)
    //    (x^2)^0.5 --> x
    //    (x^2)^z --> x^(2*z) (incorrect, z could be 1/2 or 0.5)
    //    (x^y)^(1/4) --> x^(y/4) (incorrect, y could be 4)
    // If the inner is already a rational, or the outer is an integer - we can multiply
    // safely. If the outer is a float, it can be multiplied onto rationals or floats.
    const bool can_multiply_exponents = a_pow->exponent().is_type<Rational>() ||
                                        b.is_type<Integer>() ||
                                        (a_pow->exponent().is_type<Float>() && b.is_type<Float>());
    if (can_multiply_exponents) {
      return Power::create(a_pow->base(), a_pow->exponent() * b);
    }
  }

  // Check for zeroes:
  if (is_zero(a)) {
    // 0^x -> 0  (TODO: Only true for real x)
    return Constants::Zero;
  } else if (is_zero(b)) {
    // x^0 -> 1
    return Constants::One;
  } else if (is_one(b)) {
    // x^1 -> x
    return a;
  }

  // Check if the base is a multiplication.
  // In this case, we convert to a multiplication of powers:
  // TODO: Should we only do this distribution for integer powers?
  if (const Multiplication* const mul = cast_ptr<Multiplication>(a); mul != nullptr) {
    std::vector<Expr> args;
    args.reserve(mul->arity());
    for (const Expr& arg : *mul) {
      args.push_back(Power::create(arg, b));
    }
    return Multiplication::from_operands(args);
  }
  return make_expr<Power>(a, b);
}

std::pair<Expr, Expr> as_base_and_exp(const Expr& expr) {
  if (const Power* pow = cast_ptr<Power>(expr); pow != nullptr) {
    // Return as base/exponent pair.
    return std::make_pair(pow->base(), pow->exponent());
  }
  return std::make_pair(expr, Constants::One);
}

}  // namespace math
