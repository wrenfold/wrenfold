// Copyright 2022 Gareth Cross
#include "wf/expressions/power.h"

#include <algorithm>

#include "wf/expression_visitor.h"
#include "wf/expressions/multiplication.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/integer_utils.h"

namespace wf {

// Will evaluate to true if A or B (or both) is a float, w/ the other being Integer or Rational.
// This is so we can promote integers/rationals -> float when they are combined with floats.
template <typename A, typename B>
constexpr bool is_float_and_numeric_v =
    (std::is_same_v<A, float_constant> &&
     type_list_contains_v<B, integer_constant, rational_constant>) ||
    (std::is_same_v<B, float_constant> &&
     type_list_contains_v<A, integer_constant, rational_constant>) ||
    (std::is_same_v<A, float_constant> && std::is_same_v<B, float_constant>);

static_assert(is_float_and_numeric_v<float_constant, float_constant>);
static_assert(is_float_and_numeric_v<float_constant, integer_constant>);
static_assert(is_float_and_numeric_v<rational_constant, float_constant>);
static_assert(!is_float_and_numeric_v<integer_constant, integer_constant>);
static_assert(!is_float_and_numeric_v<integer_constant, rational_constant>);

struct power_numerics_visitor {
  template <typename Base, typename Exp>
  std::optional<scalar_expr> operator()(const Base& base, const Exp& exp) {
    if constexpr (is_float_and_numeric_v<Base, Exp>) {
      return apply_float_and_numeric(base, exp);
    } else if constexpr (std::is_same_v<integer_constant, Base> &&
                         std::is_same_v<integer_constant, Exp>) {
      return apply_int_and_int(base, exp);
    } else if constexpr (std::is_same_v<rational_constant, Base> &&
                         std::is_same_v<integer_constant, Exp>) {
      return apply_rational_and_int(base, exp);
    } else if constexpr (std::is_same_v<integer_constant, Base> &&
                         std::is_same_v<rational_constant, Exp>) {
      return apply_int_and_rational(base, exp);
    } else if constexpr (std::is_same_v<complex_infinity, Base> &&
                         type_list_contains_v<Exp, integer_constant, rational_constant,
                                              float_constant>) {
      return apply_infinity_and_numeric_constant(base, exp);
    } else if constexpr (std::is_same_v<undefined, Base> || std::is_same_v<undefined, Exp>) {
      return constants::undefined;
    } else {
      return std::nullopt;
    }
  }

  // If either operand is a float, coerce the other to float:
  template <typename Base, typename Exp>
  std::enable_if_t<is_float_and_numeric_v<Base, Exp>, scalar_expr> apply_float_and_numeric(
      const Base& base, const Exp& exp) {
    if (base.is_zero() && exp.is_negative()) {
      return constants::complex_infinity;
    }
    const auto result = std::pow(static_cast<float_constant>(base).value(),
                                 static_cast<float_constant>(exp).value());
    return make_expr<float_constant>(result);
  }

  // If both operands are integers:
  static scalar_expr apply_int_and_int(const integer_constant& base, const integer_constant& exp) {
    if (exp.value() < 0) {
      if (base.is_zero()) {
        // 1 / (0)^b --> complex infinity
        return constants::complex_infinity;
      }
      // Convert a -> (1/a), then take the power:
      return apply_rational_and_int(rational_constant{1, base.value()}, -exp);
    }
    if (base.is_zero() && exp.is_zero()) {
      return constants::undefined;
    }
    // For everything else, resort to calling Pow(...), b is > 0 here:
    const auto pow = integer_power(base.value(), exp.value());
    return {pow};
  }

  // If the left operand is a rational and right operand is integer:
  static scalar_expr apply_rational_and_int(const rational_constant& base,
                                            const integer_constant& exp) {
    const auto exponent = exp.value();
    if (base.is_zero() && exponent < 0) {
      return constants::complex_infinity;
    }
    if (base.is_zero() && exp.is_zero()) {
      return constants::undefined;
    }
    const auto abs_exponent = static_cast<std::uint64_t>(abs(exponent));
    const auto n = integer_power(base.numerator(), abs_exponent);
    const auto d = integer_power(base.denominator(), abs_exponent);
    if (exponent >= 0) {
      return scalar_expr(rational_constant{n, d});
    } else {
      // Flip the rational:
      return scalar_expr(rational_constant{d, n});
    }
  }

  // A power of the form base**exponent, where both values are integers.
  struct int_base_and_exponent {
    checked_int base{0};
    checked_int exponent{0};
  };

  // If the left operand is integer, and the right is rational:
  static scalar_expr apply_int_and_rational(const integer_constant& base,
                                            const rational_constant& exp) {
    WF_ASSERT_GREATER(exp.denominator(), 0, "Rational must have positive denominator");
    if (base.value() == 1) {
      return constants::one;
    } else if (base.is_zero()) {
      if (exp.is_zero()) {
        // 0^0 --> undefined
        return constants::undefined;
      } else if (exp.is_negative()) {
        return constants::complex_infinity;
      } else {
        return constants::zero;
      }
    }

    // Factorize the integer exponent (positive part only) into primes:
    const std::vector<prime_factor> factors = compute_prime_factors(abs(base.value()));

    // This is a map from exponent denominator to an integer base and exponent.
    // So, for example, n**(p/q) becomes entry: {q: [base: n, exponent: p]}
    std::unordered_map<checked_int, int_base_and_exponent, hash_struct<checked_int>> exp_to_base{};

    // Iterate over factors and put them into canonical form:
    // negative example:
    //    2 ^ (-3/7) --> (1/2) * 2 ^ (4/7)
    //    2 ^ (-12/7) --> (1/2) * 2 ^ (-5/7) --> (1/4) * 2 ^ (2/7)
    // positive example:
    //    2 ^ (18/7) --> 4 * 2 ^ (4/7)
    // See https://arxiv.org/pdf/1302.2169.pdf for examples of canonical forms.
    //
    // In this implementation, we group terms in the resulting product by the _denominator_ of their
    // exponent. So 2**(1/7) * 3**(2/7) will be converted to: (2 * 3**2)**(1/7)
    // We bring out any integer parts and multiply them into `rational_coeff`, and all exponents are
    // made into positive rationals.
    rational_constant rational_coeff{1, 1};
    for (const prime_factor& factor : factors) {
      // Multiply the power by the rational to get the exponent applied to this prime factor.
      // Primes may be repeated, so `f.exponent` is not necessarily one.
      const rational_constant actual_exp =
          exp * static_cast<rational_constant>(integer_constant{factor.exponent});

      // Factorize the exponent: x^(int_part + frac_part) --> x^int_part * x^frac_part
      // Fractional part will never be negative (we add to the integer part to make this the case).
      const auto [integer_part, fractional_part] = factorize_rational_exponent(actual_exp);
      WF_ASSERT(!fractional_part.is_negative() &&
                    (fractional_part.is_zero() || !fractional_part.is_integer()),
                "fractional_part = {}", fractional_part);

      // Apply the integer part to the rational coefficient:
      if (integer_part >= 0) {
        rational_coeff =
            rational_coeff * rational_constant{integer_power(factor.base, integer_part), 1};
      } else {
        rational_coeff =
            rational_coeff * rational_constant{1, integer_power(factor.base, -integer_part)};
      }

      if (!fractional_part.is_zero()) {
        if (const auto [it, was_inserted] = exp_to_base.emplace(
                fractional_part.denominator(),
                int_base_and_exponent{factor.base, fractional_part.numerator()});
            !was_inserted) {
          // Find the greatest common divistor of both numerators, and make that the
          // new numerator.
          const checked_int shared_numerator =
              gcd(it->second.exponent, fractional_part.numerator());
          it->second.base =
              integer_power(it->second.base, it->second.exponent / shared_numerator) *
              integer_power(factor.base, fractional_part.numerator() / shared_numerator);
          it->second.exponent = shared_numerator;
        }
      }
    }

    std::vector<scalar_expr> operands{};
    operands.reserve(exp_to_base.size() + 2);

    // If a is negative, we add another factor (-1) here:
    if (base.is_negative()) {
      const auto [integer_part, fractional_part] = factorize_rational_exponent(exp);
      if (!integer_part.is_even()) {
        rational_coeff = rational_coeff * -1;
      }
      if (fractional_part == rational_constant{1, 2}) {
        // (-1)**(1/2) --> i
        operands.push_back(constants::imaginary_unit);
      } else if (!fractional_part.is_zero()) {
        // If there is already a term where the exponent is `fractional_part`, put the -1 in there.
        // Otherwise create a new separate term for it.
        if (const auto it = exp_to_base.find(fractional_part.denominator());
            it != exp_to_base.end() && it->second.exponent == fractional_part.numerator()) {
          it->second.base *= -1;
        } else {
          operands.emplace_back(std::in_place_type_t<power>(), constants::negative_one,
                                scalar_expr(fractional_part));
        }
      }
    }

    for (const auto [exponent_denominator, base_and_exp] : exp_to_base) {
      operands.emplace_back(
          std::in_place_type_t<power>(), scalar_expr(base_and_exp.base),
          scalar_expr(rational_constant{base_and_exp.exponent, exponent_denominator}));
    }
    if (!rational_coeff.is_one()) {
      operands.emplace_back(rational_coeff);
    }
    return multiplication::from_operands(operands);
  }

  template <typename T>
  static scalar_expr apply_infinity_and_numeric_constant(const complex_infinity&, const T& exp) {
    if (exp.is_positive()) {
      return constants::complex_infinity;
    } else if (exp.is_negative()) {
      return constants::zero;
    } else {
      // infinity ^ 0
      return constants::undefined;
    }
  }
};

// Result of `imaginary_unit_power`.
enum class imaginary_unit_power_result {
  // Result is +1
  one,
  // Result is -1
  negative_one,
  // Result is `i`
  i,
  // Result is -i
  negative_i,
};

// Determine the result of i**n where `n` is an integer and `i` is the imaginary unit.
constexpr imaginary_unit_power_result imaginary_unit_power(const integer_constant exp) noexcept {
  // There might be a simpler way to write this logic, this is my first pass at it.
  const integer_constant exp_over_two{exp.value() / 2};
  if (exp.is_even()) {
    return exp_over_two.is_even() ? imaginary_unit_power_result::one
                                  : imaginary_unit_power_result::negative_one;
  } else if (exp.is_positive()) {
    if (exp_over_two.is_even()) {
      return imaginary_unit_power_result::i;
    } else {
      return imaginary_unit_power_result::negative_i;
    }
  } else {
    // exp is negative and odd
    if (exp_over_two.is_even()) {
      return imaginary_unit_power_result::negative_i;
    } else {
      return imaginary_unit_power_result::i;
    }
  }
}

// Visitor to handle i**integer or i**rational.
struct power_imaginary_visitor {
  template <typename T,
            typename = enable_if_does_not_contain_type_t<T, integer_constant, rational_constant>>
  std::optional<scalar_expr> operator()(const T&) const noexcept {
    return std::nullopt;
  }

  std::optional<scalar_expr> operator()(const integer_constant exp) const {
    switch (imaginary_unit_power(exp)) {
      case imaginary_unit_power_result::one:
        return constants::one;
      case imaginary_unit_power_result::negative_one:
        return constants::negative_one;
      case imaginary_unit_power_result::i:
        return constants::imaginary_unit;
      case imaginary_unit_power_result::negative_i:
      default: {
        // We explicitly call multiplication constructor here because we don't want go through
        // operator* and accidentically recurse back into the pow(...) logic.
        static const auto negative_i =
            make_expr<multiplication>(constants::negative_one, constants::imaginary_unit);
        return negative_i;
      }
    }
  }

  std::optional<scalar_expr> operator()(const rational_constant& exp,
                                        const scalar_expr& exp_abstract) const {
    const auto [integer_part, fractional_part] = factorize_rational_exponent(exp);
    WF_ASSERT(!fractional_part.is_negative(), "fractional_part = {}", fractional_part);

    switch (imaginary_unit_power(integer_constant{integer_part})) {
      case imaginary_unit_power_result::one:
        // i**fractional_part
        return make_expr<power>(constants::imaginary_unit, scalar_expr{fractional_part});
      case imaginary_unit_power_result::negative_one:
        // -1 * i**fractional_part
        return make_expr<multiplication>(
            constants::negative_one,
            make_expr<power>(constants::imaginary_unit, scalar_expr{fractional_part}));
      case imaginary_unit_power_result::i: {
        // i**(fractional_part + 1)
        if (integer_part == 1) {
          // No need to re-construct the rational exponent:
          return make_expr<power>(constants::imaginary_unit, exp_abstract);
        } else {
          return make_expr<power>(constants::imaginary_unit,
                                  scalar_expr{fractional_part + rational_constant(1, 1)});
        }
      }
      case imaginary_unit_power_result::negative_i:
      default: {
        // -1 * i**(fractional_part + 1)
        return make_expr<multiplication>(
            constants::negative_one,
            make_expr<power>(constants::imaginary_unit,
                             scalar_expr{fractional_part + rational_constant(1, 1)}));
      }
    }
  }
};

static bool magnitude_less_than_one(const scalar_expr& value) {
  if (const rational_constant* r = get_if<const rational_constant>(value);
      r != nullptr && r->is_proper()) {
    return true;
  } else if (const float_constant* f = get_if<const float_constant>(value);
             f != nullptr && std::abs(f->value()) < 1.0) {
    return true;
  }
  return false;
}

// We want to avoid doing incorrect simplifications like:
//    (x^2)^(1/2) --> x (incorrect, loses the sign of x)
//    (x^2)^0.5
//    (x^2)^z --> x^(2*z) (incorrect, z could be 1/2 or 0.5)
//    (x^y)^(1/4) --> x^(y/4) (incorrect, y could be 4)
static bool can_multiply_exponents(const power& base_pow, const scalar_expr& outer_exp) {
  // If the inner power is a rational:
  // For example, it is valid to do (inner exponent is proper):
  //  (x**(1/4))**y  --> x**(y/4)
  // But, in general, not valid to do:
  //  (x**(5/3))**y --> x**(5*y/3)
  // Unless:
  //  - `y` is an integer
  //  - `x` is real and non-negative
  if (const scalar_expr& inner_exp = base_pow.exponent(); magnitude_less_than_one(inner_exp)) {
    return true;
  }
  if (outer_exp.is_type<integer_constant>()) {
    return true;
  }
  if (const number_set base_set = determine_numeric_set(base_pow.base());
      base_set == number_set::real_non_negative || base_set == number_set::real_positive) {
    return true;
  }
  return false;
}

// Split `mul` into non-negative terms and the rest. Apply distribution of rational exponent to
// all the non-negative terms.
static std::optional<scalar_expr> maybe_distribute_rational_exponent(const multiplication& mul,
                                                                     const scalar_expr& exp) {
  std::vector<scalar_expr> non_negative_terms, remaining_terms;
  non_negative_terms.reserve(mul.size());
  remaining_terms.reserve(mul.size());

  for (const scalar_expr& term : mul) {
    if (const auto set = determine_numeric_set(term);
        set == number_set::real_non_negative || set == number_set::real_positive) {
      non_negative_terms.push_back(pow(term, exp));
    } else {
      remaining_terms.push_back(term);
    }
  }

  if (non_negative_terms.empty()) {
    return std::nullopt;
  } else if (remaining_terms.empty()) {
    return multiplication::from_operands(non_negative_terms);
  } else {
    return multiplication::from_operands(non_negative_terms) *
           pow(multiplication::from_operands(remaining_terms), exp);
  }
}

scalar_expr power::create(scalar_expr base, scalar_expr exp) {
  if (auto result = pow_maybe_simplify(base, exp); result.has_value()) {
    return *std::move(result);
  }
  return make_expr<power>(std::move(base), std::move(exp));
}

std::optional<scalar_expr> pow_maybe_simplify(const scalar_expr& base, const scalar_expr& exp) {
  // Check for numeric quantities.
  if (std::optional<scalar_expr> numeric_pow = visit_binary(base, exp, power_numerics_visitor{});
      numeric_pow.has_value()) {
    return numeric_pow;
  }

  if ((is_one(base) || is_negative_one(base)) && is_complex_infinity(exp)) {
    // 1^∞ (for any type of infinity) is undefined
    return constants::undefined;
  }

  // The base is the imaginary constant.
  if (is_i(base)) {
    if (std::optional<scalar_expr> imaginary_pow = visit(exp, power_imaginary_visitor{});
        imaginary_pow.has_value()) {
      return imaginary_pow;
    }
  }

  // Check if the base is itself a power:
  if (const power* a_pow = get_if<const power>(base);
      a_pow != nullptr && can_multiply_exponents(*a_pow, exp)) {
    return power::create(a_pow->base(), a_pow->exponent() * exp);
  }

  // TODO: Check for `b > 0`, then 0**b --> 0
  if (is_complex_infinity(exp)) {
    return constants::undefined;
  } else if (is_zero(exp)) {
    // x^0 -> 1
    return constants::one;
  } else if (is_one(exp)) {
    // x^1 -> x
    return base;
  }

  // Check if the base is a multiplication and the exponent is an integer.
  // In this case, we convert to a multiplication of powers.
  if (const multiplication* const mul = get_if<const multiplication>(base); mul != nullptr) {
    if (exp.is_type<integer_constant>()) {
      const auto args = transform_map<std::vector>(
          *mul, [&exp](const scalar_expr& arg) { return power::create(arg, exp); });
      return multiplication::from_operands(args);
    } else if (exp.is_type<rational_constant>()) {
      if (auto result = maybe_distribute_rational_exponent(*mul, exp); result.has_value()) {
        return result;
      }
    }
  }
  return std::nullopt;
}

scalar_expr pow(scalar_expr base, scalar_expr exp) {
  return power::create(std::move(base), std::move(exp));
}

std::pair<scalar_expr, scalar_expr> as_base_and_exp(const scalar_expr& expr) {
  if (const power* pow = get_if<const power>(expr); pow != nullptr) {
    // Return as base/exponent pair.
    return std::make_pair(pow->base(), pow->exponent());
  }
  return std::make_pair(expr, constants::one);
}

}  // namespace wf
