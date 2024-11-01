// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/multiplication.h"

#include <algorithm>

#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"
#include "wf/utility_visitors.h"

namespace wf {

// Define rules for multiplying constants:
struct multiply_numeric_constants {
  constexpr integer_constant operator()(const integer_constant a, const integer_constant b) const {
    return integer_constant{a.value() * b.value()};
  }

  // Promote int to rational:
  template <typename B,
            typename = enable_if_contains_type_t<B, integer_constant, rational_constant>>
  constexpr rational_constant operator()(const rational_constant a, const B b) const {
    return a * static_cast<rational_constant>(b);
  }

  // Promote int and rational to float:
  template <typename B, typename = enable_if_contains_type_t<B, float_constant, integer_constant,
                                                             rational_constant>>
  constexpr multiplication_parts::constant_coeff operator()(const float_constant a,
                                                            const B b) const {
    if (const float_constant prod = a * static_cast<float_constant>(b); !prod.is_zero()) {
      return prod;
    }
    // Float multiplications that work out to zero should be converted to integer zero.
    return integer_constant{0};
  }

  // Anything times undefined is undefined.
  template <typename B>
  constexpr undefined operator()(const undefined, const B) const noexcept {
    return {};
  }

  // Anything times complex infinity is complex infinity, except undefined, and zero.
  template <typename B, typename = enable_if_does_not_contain_type_t<B, undefined>>
  constexpr multiplication_parts::constant_coeff operator()(const complex_infinity,
                                                            const B b) const noexcept {
    if constexpr (type_list_contains_v<B, integer_constant, float_constant, rational_constant>) {
      if (b.is_zero()) {
        return undefined{};
      }
    }
    return complex_infinity{};
  }

  // Disallow implicit conversion to the variant with enable_if guard.
  template <typename V, typename T,
            typename = enable_if_same_t<V, multiplication_parts::constant_coeff>>
  constexpr multiplication_parts::constant_coeff operator()(const V& a, const T b) const {
    return std::visit(
        [b](auto a_concrete) constexpr -> multiplication_parts::constant_coeff {
          if constexpr (is_invocable_v<multiply_numeric_constants, decltype(a_concrete), T>) {
            return multiply_numeric_constants{}(a_concrete, b);
          } else {
            return multiply_numeric_constants{}(b, a_concrete);
          }
        },
        a);
  }
};

inline scalar_expr maybe_new_mul(multiplication::container_type&& terms) {
  if (terms.empty()) {
    return constants::one;
  } else if (terms.size() == 1) {
    return std::move(terms.front());
  } else {
    return make_expr<multiplication>(std::move(terms));
  }
}

static scalar_expr multiply_into_addition(const addition& add,
                                          const scalar_expr& numerical_constant) {
  const addition::container_type add_args = transform_map<addition::container_type>(
      add, [&](const scalar_expr& f) { return f * numerical_constant; });
  return addition::from_operands(add_args);  // TODO: make this a move.
}

std::vector<scalar_expr> multiplication::sorted_terms() const {
  std::vector<scalar_expr> result{begin(), end()};
  std::sort(result.begin(), result.end(), expression_order_struct{});
  return result;
}

scalar_expr multiplication::from_operands(const absl::Span<const scalar_expr> args) {
  if (args.empty()) {
    throw invalid_argument_error("Need at least one operand to construct multiplication.");
  }
  if (args.size() < 2) {
    return args.front();
  }

  // Check for `addition * constant`.
  // Combinations for > 2 args are handled by `create_multiplication`.
  if (args.size() == 2) {
    if (const addition* add = get_if<const addition>(args[0]);
        add && args[1].is_type<integer_constant, rational_constant, float_constant>()) {
      return multiply_into_addition(*add, args[1]);
    } else if (add = get_if<const addition>(args[1]);
               add && args[0].is_type<integer_constant, float_constant, rational_constant>()) {
      return multiply_into_addition(*add, args[0]);
    }
  }

  multiplication_parts parts{args.size()};
  for (const scalar_expr& term : args) {
    parts.multiply_term(term);
  }
  parts.normalize_coefficients();
  return std::move(parts).create_multiplication();
}

void multiplication::sort_terms() {
  std::sort(terms_.begin(), terms_.end(), [](const scalar_expr& a, const scalar_expr& b) {
    if (a.hash() < b.hash()) {
      return true;
    } else if (a.hash() > b.hash()) {
      return false;
    } else {
      return determine_order(a, b) == relative_order::less_than;
    }
  });
}

void multiplication_parts::operator()(const multiplication& mul) {
  for (const scalar_expr& expr : mul) {
    // Recursively add multiplications:
    visit(expr, *this);
  }
}

void multiplication_parts::operator()(const power& pow) {
  insert_power(pow.base(), pow.exponent());
}

template <typename T, typename>
void multiplication_parts::operator()(const T& value, const scalar_expr& input_expression) {
  if constexpr (std::is_same_v<T, integer_constant>) {
    if (factorize_integers_) {
      // Factorize integers into primes:
      insert_integer_factors(compute_prime_factors(value.value()), true);
    } else {
      // Promote integers to rationals and multiply them onto `rational_coeff`.
      coeff_ = multiply_numeric_constants{}(coeff_, value);
    }
  } else if constexpr (std::is_same_v<T, rational_constant>) {
    if (factorize_integers_) {
      insert_integer_factors(compute_prime_factors(value.numerator()), true);
      insert_integer_factors(compute_prime_factors(value.denominator()), false);
    } else {
      coeff_ = multiply_numeric_constants{}(coeff_, value);
    }
  } else if constexpr (type_list_contains_v<T, type_list_from_variant_t<constant_coeff>>) {
    coeff_ = multiply_numeric_constants{}(coeff_, value);
  } else {
    // Everything else: Just raise the power by +1.
    insert_power(input_expression, constants::one);
  }
}

void multiplication_parts::insert_power(const scalar_expr& base, const scalar_expr& exponent) {
  if (const auto [it, was_inserted] = terms_.emplace(base, exponent); !was_inserted) {
    scalar_expr updated_exponent = it->second + exponent;
    // There's a chance that by updating the exponent, our base will turn into a multiplication.
    // This can only occur if the base is a multiplication or a power, for example:
    // (x*y)**(1/2) * (x*y)**(1/2) --> (x*y)
    // (x**2)**(1 + y) * (x**2)**(-y) --> (x**2)**1 --> x**2
    if (base.is_type<multiplication, power>()) {
      if (const std::optional<scalar_expr> simplified =
              pow_maybe_simplify(it->first, updated_exponent);
          simplified.has_value() && simplified->is_type<multiplication, power>()) {
        // If it did simplify, we need to update `terms` with the result:
        terms_.erase(it);
        visit(*simplified, *this);
        return;  //  Don't update it->second below.
      }
    }
    it->second = std::move(updated_exponent);
  }
}

template <typename T>
void multiplication_parts::insert_integer_factors(const T& factors, const bool positive) {
  for (const prime_factor& factor : factors) {
    scalar_expr base{factor.base};
    scalar_expr exponent{positive ? factor.exponent : -factor.exponent};
    if (const auto [it, was_inserted] = terms_.emplace(std::move(base), exponent); !was_inserted) {
      it->second = it->second + exponent;
    }
  }
}

multiplication_parts::multiplication_parts(const multiplication& mul, const bool factorize_integers)
    : multiplication_parts(mul.size(), factorize_integers) {
  for (const scalar_expr& expr : mul) {
    multiply_term(expr);
  }
  normalize_coefficients();
}

void multiplication_parts::multiply_term(const scalar_expr& arg) { visit(arg, *this); }

void multiplication_parts::normalize_coefficients() {
  // Nuke anything w/ a zero exponent.
  map_erase_if(terms_, [](const auto& pair) { return is_zero(pair.second); });
}

scalar_expr multiplication_parts::create_multiplication() && {
  multiplication::container_type args{};
  multiplication_parts::constant_coeff constant_coefficient = coeff_;

  // Convert into a vector of powers, and sort into canonical order:
  for (auto& [base, exp] : terms_) {
    auto pow = power::create(base, std::move(exp));

    // The power may have produced a numerical coefficient:
    auto [pow_coeff, mul] = as_coeff_and_mul(pow);

    // If there is a non-unit coefficient resulting from the power, multiply it onto the constant.
    // ReSharper disable once CppTooWideScope
    const bool stripped_coefficient = visit(pow_coeff, [&](const auto& numeric) {
      using T = std::decay_t<decltype(numeric)>;
      if constexpr (type_list_contains_v<T, integer_constant, rational_constant, float_constant>) {
        constant_coefficient = multiply_numeric_constants{}(constant_coefficient, numeric);
        return true;
      } else {
        return false;
      }
    });

    if (stripped_coefficient) {
      if (!is_one(mul)) {
        args.push_back(std::move(mul));
      }
    } else {
      // Leave the result of power intact.
      args.push_back(std::move(pow));
    }
  }

  scalar_expr constant_coeff_expr =
      overloaded_visit(constant_coefficient, [](const auto x) { return scalar_expr(x); });

  if (is_zero(constant_coeff_expr) || is_undefined(constant_coeff_expr)) {
    return constant_coeff_expr;
  }

  if (args.size() == 1) {
    // Other than the numeric value, we only have one expression.
    // If this term is an addition, distribute the numerical value over the addition:
    if (const addition* add = get_if<const addition>(args[0]);
        add != nullptr && !is_one(constant_coeff_expr)) {
      return multiply_into_addition(*add, constant_coeff_expr);
    }
  }
  if (!is_one(constant_coeff_expr)) {
    args.push_back(std::move(constant_coeff_expr));
  }
  return maybe_new_mul(std::move(args));
}

// For multiplications, we need to break the expression up.
static std::pair<scalar_expr, scalar_expr> split_multiplication(const multiplication& mul,
                                                                const scalar_expr& mul_abstract) {
  multiplication::container_type numerics{};
  multiplication::container_type remainder{};
  for (const scalar_expr& expr : mul) {
    if (is_numeric(expr)) {
      numerics.push_back(expr);
    } else {
      remainder.push_back(expr);
    }
  }
  if (numerics.empty()) {
    // No point making a new multiplication:
    return std::make_pair(constants::one, mul_abstract);
  }
  auto coeff = maybe_new_mul(std::move(numerics));
  auto multiplicand = maybe_new_mul(std::move(remainder));
  return std::make_pair(std::move(coeff), std::move(multiplicand));
}

std::pair<scalar_expr, scalar_expr> as_coeff_and_mul(const scalar_expr& expr) {
  return visit(expr, [&expr](const auto& x) -> std::pair<scalar_expr, scalar_expr> {
    using T = std::decay_t<decltype(x)>;
    if constexpr (type_list_contains_v<T, integer_constant, rational_constant, float_constant>) {
      // Numerical values are always the coefficient:
      return std::make_pair(expr, constants::one);
    } else if constexpr (std::is_same_v<T, multiplication>) {
      // Handle multiplication. We do a faster path for a common case:
      if (const multiplication& mul = x; mul.size() == 2) {
        const bool first_is_numeric =
            mul[0].is_type<integer_constant, rational_constant, float_constant>();
        const bool second_is_numeric =
            mul[1].is_type<integer_constant, rational_constant, float_constant>();
        if (first_is_numeric && !second_is_numeric) {
          return std::make_pair(mul[0], mul[1]);
        } else if (!first_is_numeric && second_is_numeric) {
          return std::make_pair(mul[1], mul[0]);
        }
      }
      return split_multiplication(x, expr);
    } else {
      return std::make_pair(constants::one, expr);
    }
  });
}

multiplication_format_parts get_formatting_info(const multiplication& mul) {
  multiplication_format_parts result{};

  // Sort into canonical order:
  absl::InlinedVector<scalar_expr, 8> terms{mul.begin(), mul.end()};
  std::sort(terms.begin(), terms.end(), [](const scalar_expr& a, const scalar_expr& b) {
    const auto a_base_exp = as_base_and_exp(a);
    const auto b_base_exp = as_base_and_exp(b);
    return order_by(std::get<0>(a_base_exp), std::get<0>(b_base_exp))
        .and_then_by(std::get<1>(a_base_exp), std::get<1>(b_base_exp))
        .is_less_than();
  });

  std::size_t sign_count = 0;
  for (const scalar_expr& expr : terms) {
    // Extract rationals:
    if (const rational_constant* const rational = get_if<const rational_constant>(expr);
        rational != nullptr) {
      if (const auto abs_num = abs(rational->numerator()); abs_num != 1) {
        // Don't put redundant ones into the numerator for rationals of the form 1/n.
        result.numerator.emplace_back(integer_constant{abs_num});
      }
      result.denominator.emplace_back(integer_constant{rational->denominator()});

      if (rational->numerator() < 0) {
        // If negative, increase the sign count.
        ++sign_count;
      }
    } else if (const integer_constant* const integer = get_if<const integer_constant>(expr);
               integer != nullptr) {
      if (integer->value() != 1 && integer->value() != -1) {
        result.numerator.emplace_back(integer->abs());
      }
      if (integer->value() < 0) {
        ++sign_count;
      }
    } else if (const float_constant* const f = get_if<const float_constant>(expr); f != nullptr) {
      result.numerator.emplace_back(f->abs());
      if (f->value() < 0) {
        ++sign_count;
      }
    } else {
      // This isn't a numeric value, so break it into base and exponent:
      auto [base, exponent] = as_base_and_exp(expr);
      // Sort into numerator and denominator, depending on sign of the exponent:
      const auto [coeff, _] = as_coeff_and_mul(exponent);
      // See if the exponent seems negative:
      if (const bool is_negative_exp = is_negative_number(coeff); is_negative_exp) {
        if (is_negative_one(exponent)) {
          result.denominator.emplace_back(power{std::move(base), constants::one});
        } else {
          // Flip the sign and create a new power.
          result.denominator.emplace_back(power{std::move(base), -exponent});
        }
      } else {
        result.numerator.emplace_back(power{std::move(base), std::move(exponent)});
      }
    }
  }

  result.is_negative = static_cast<bool>(sign_count & 1);  //  Even = positive, Odd = negative
  if (result.numerator.empty()) {
    // If all powers were negative, and we had only a rational, the numerator may be empty:
    result.numerator.emplace_back(integer_constant{1});
  }
  return result;
}

}  // namespace wf
