// Copyright 2022 Gareth Cross
#include "wf/expressions/power.h"

#include <algorithm>

#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/integer_utils.h"
#include "wf/visitor_impl.h"

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
    } else if constexpr (std::is_same_v<Infinity, A> &&
                         type_list_contains_type_v<B, Integer, Rational>) {
      return apply_infinity_and_rational(a, static_cast<Rational>(b));
    } else if constexpr (std::is_same_v<Infinity, A> && std::is_same_v<B, Float>) {
      return apply_infinity_and_float(a, b);
    } else if constexpr (std::is_same_v<Undefined, A> || std::is_same_v<Undefined, B>) {
      return constants::undefined;
    } else {
      return std::nullopt;
    }
  }

  // If either operand is a float, coerce the other to float:
  template <typename A, typename B>
  std::enable_if_t<is_float_and_numeric_v<A, B>, Expr> apply_float_and_numeric(const A& a,
                                                                               const B& b) {
    if (a.is_zero() && b.is_negative()) {
      return constants::complex_infinity;
    }
    const auto result =
        std::pow(static_cast<Float>(a).get_value(), static_cast<Float>(b).get_value());
    return make_expr<Float>(result);
  }

  // If both operands are integers:
  Expr apply_int_and_int(const Integer& a, const Integer& b) {
    if (b.get_value() < 0) {
      if (a.is_zero()) {
        // 1 / (0)^b --> complex infinity
        return constants::complex_infinity;
      }
      // Convert a -> (1/a), then take the power:
      return apply_rational_and_int(Rational{1, a.get_value()}, -b);
    }
    if (a.is_zero() && b.is_zero()) {
      return constants::undefined;
    }
    // For everything else, resort to calling Pow(...), b is > 0 here:
    const auto pow = integer_power(a.get_value(), b.get_value());
    return Integer::create(pow);
  }

  // If the left operand is a rational and right operand is integer:
  Expr apply_rational_and_int(const Rational& a, const Integer& b) {
    const auto exponent = b.get_value();
    if (a.is_zero() && exponent < 0) {
      return constants::complex_infinity;
    }
    if (a.is_zero() && b.is_zero()) {
      return constants::undefined;
    }
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
    WF_ASSERT_GREATER(b.denominator(), 0, "Rational must have positive denominator");
    if (a.get_value() == 1) {
      return constants::one;
    } else if (a.get_value() == 0) {
      if (b.is_zero()) {
        // 0^0 --> undefined
        return constants::undefined;
      } else if (b.is_negative()) {
        return constants::complex_infinity;
      } else {
        return constants::zero;
      }
    }

    // Factorize the integer into primes:
    const std::vector<prime_factor> factors = compute_prime_factors(a.get_value());
    WF_ASSERT(std::is_sorted(factors.begin(), factors.end(),
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
    for (const prime_factor& f : factors) {
      // Multiply the power by the rational to get the exponent applied to this prime factor:
      const Rational actual_exp = b * static_cast<Rational>(Integer{f.exponent});
      WF_ASSERT_GREATER(f.exponent, 0);  //  Exponents must be >= 1 in this context.

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

  Expr apply_infinity_and_rational(const Infinity&, const Rational& r) const {
    if (r.numerator() > 0) {
      return constants::complex_infinity;
    } else if (r.numerator() < 0) {
      return constants::zero;
    } else {
      // infinity ^ 0
      return constants::undefined;
    }
  }

  Expr apply_infinity_and_float(const Infinity&, const Float& f) const {
    if (f.get_value() > 0) {
      return constants::complex_infinity;
    } else if (f.get_value() < 0) {
      return constants::zero;
    } else {
      // infinity ^ 0.0
      return constants::undefined;
    }
  }
};

static bool magnitude_less_than_one(const Expr& value) {
  if (const Rational* r = cast_ptr<Rational>(value); r != nullptr && r->is_proper()) {
    return true;
  } else if (const Float* f = cast_ptr<Float>(value);
             f != nullptr && std::abs(f->get_value()) < 1.0) {
    return true;
  }
  return false;
}

// We want to avoid doing incorrect simplifications like:
//    (x^2)^(1/2) --> x (incorrect, loses the sign of x)
//    (x^2)^0.5
//    (x^2)^z --> x^(2*z) (incorrect, z could be 1/2 or 0.5)
//    (x^y)^(1/4) --> x^(y/4) (incorrect, y could be 4)
static bool can_multiply_exponents(const Power& base_pow, const Expr& outer_exp) {
  // If the inner power is a rational:
  // For example, it is valid to do (inner exponent is proper):
  //  (x**(1/4))**y  --> x**(y/4)
  // But, in general, not valid to do:
  //  (x**(5/3))**y --> x**(5*y/3)
  // Unless:
  //  - `y` is an integer
  //  - `x` is real and non-negative
  const Expr& inner_exp = base_pow.exponent();
  if (magnitude_less_than_one(inner_exp)) {
    return true;
  }
  if (outer_exp.is_type<Integer>()) {
    return true;
  }
  const NumberSet base_set = determine_numeric_set(base_pow.base());
  if (base_set == NumberSet::RealNonNegative || base_set == NumberSet::RealPositive) {
    return true;
  }
  return false;
}

Expr Power::create(Expr a, Expr b) {
  // Check for numeric quantities.
  std::optional<Expr> numeric_pow = visit_binary(a, b, PowerNumerics{});
  if (numeric_pow) {
    return *numeric_pow;
  }

  if ((is_one(a) || is_negative_one(a)) && is_complex_infinity(b)) {
    // 1^∞ (for any type of infinity) is undefined
    return constants::undefined;
  }

  // Check if the base is itself a power:
  if (const Power* a_pow = cast_ptr<Power>(a); a_pow != nullptr) {
    if (can_multiply_exponents(*a_pow, b)) {
      return Power::create(a_pow->base(), a_pow->exponent() * b);
    }
  }

  // Check for zeroes:
  if (is_zero(a)) {
    if (is_complex_infinity(b)) {
      return constants::undefined;
    }
    // TODO: Check for `b > 0`, then 0**b --> 0
  } else if (is_zero(b)) {
    // x^0 -> 1
    return constants::one;
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
  return make_expr<Power>(std::move(a), std::move(b));
}

std::pair<Expr, Expr> as_base_and_exp(const Expr& expr) {
  if (const Power* pow = cast_ptr<Power>(expr); pow != nullptr) {
    // Return as base/exponent pair.
    return std::make_pair(pow->base(), pow->exponent());
  }
  return std::make_pair(expr, constants::one);
}

}  // namespace math
