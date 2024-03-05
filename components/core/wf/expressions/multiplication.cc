// Copyright 2022 Gareth Cross
#include "wf/expressions/multiplication.h"

#include <algorithm>

#include "wf/common_visitors.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"

namespace wf {

inline scalar_expr maybe_new_mul(multiplication::container_type&& terms) {
  if (terms.empty()) {
    return constants::one;
  } else if (terms.size() == 1) {
    return std::move(terms.front());
  } else {
    return make_expr<multiplication>(std::move(terms));
  }
}

static inline scalar_expr multiply_into_addition(const addition& add,
                                                 const scalar_expr& numerical_constant) {
  addition::container_type add_args{};
  add_args.reserve(add.size());
  std::transform(
      add.begin(), add.end(), std::back_inserter(add_args),
      [&numerical_constant](const scalar_expr& add_term) { return add_term * numerical_constant; });
  return addition::from_operands(add_args);  // TODO: make this a move!
}

std::vector<scalar_expr> multiplication::sorted_terms() const {
  std::vector<scalar_expr> result{begin(), end()};
  std::sort(result.begin(), result.end(), expression_order_struct{});
  return result;
}

scalar_expr multiplication::from_operands(absl::Span<const scalar_expr> args) {
  WF_ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  if (std::any_of(args.begin(), args.end(), &is_undefined)) {
    return constants::undefined;
  }

  // TODO: this simplification doesn't always work because there might be multiple
  // integer/rational/float terms.
  if (args.size() == 2) {
    if (const addition* add = cast_ptr<const addition>(args[0]);
        add && args[1].is_type<integer_constant, rational_constant, float_constant>()) {
      return multiply_into_addition(*add, args[1]);
    } else if (add = cast_ptr<const addition>(args[1]);
               add && args[0].is_type<integer_constant, float_constant, rational_constant>()) {
      return multiply_into_addition(*add, args[0]);
    }
  }

  // Now canonicalize the arguments:
  multiplication_parts builder{args.size()};
  for (const scalar_expr& term : args) {
    builder.multiply_term(term);
  }
  builder.normalize_coefficients();
  return builder.create_multiplication();
}

template <bool FactorizeIntegers>
struct multiply_visitor {
  explicit multiply_visitor(multiplication_parts& builder) : builder(builder) {}

  void insert_integer_factors(const std::vector<prime_factor>& factors, const bool positive) const {
    for (const prime_factor& factor : factors) {
      scalar_expr base{factor.base};
      scalar_expr exponent{positive ? factor.exponent : -factor.exponent};
      if (const auto [it, was_inserted] = builder.terms.emplace(std::move(base), exponent);
          !was_inserted) {
        it->second = it->second + exponent;
      }
    }
  }

  template <typename T>
  void operator()(const T& arg, const scalar_expr& input_expression) {
    if constexpr (std::is_same_v<T, multiplication>) {
      for (const scalar_expr& expr : arg) {
        // Recursively add multiplications:
        visit(expr, [this, &expr](const auto& x) { this->operator()(x, expr); });
      }
    } else if constexpr (std::is_same_v<T, power>) {
      const power& arg_pow = arg;
      // Try to insert. If it already exists, replace the exponent with the sum of exponents:
      const auto [it, was_inserted] = builder.terms.emplace(arg_pow.base(), arg_pow.exponent());
      if (!was_inserted) {
        it->second = it->second + arg_pow.exponent();
      }
    } else if constexpr (std::is_same_v<T, integer_constant>) {
      if constexpr (FactorizeIntegers) {
        // Factorize integers into primes:
        const auto factors = compute_prime_factors(arg.get_value());
        insert_integer_factors(factors, true);
      } else {
        // Promote integers to rationals and multiply them onto `rational_coeff`.
        builder.rational_coeff = builder.rational_coeff * static_cast<rational_constant>(arg);
      }
    } else if constexpr (std::is_same_v<T, rational_constant>) {
      if constexpr (FactorizeIntegers) {
        const auto num_factors = compute_prime_factors(arg.numerator());
        const auto den_factors = compute_prime_factors(arg.denominator());
        insert_integer_factors(num_factors, true);
        insert_integer_factors(den_factors, false);
      } else {
        builder.rational_coeff = builder.rational_coeff * arg;
      }
    } else if constexpr (std::is_same_v<T, float_constant>) {
      if (!builder.float_coeff.has_value()) {
        builder.float_coeff = arg;
      } else {
        builder.float_coeff = (*builder.float_coeff) * arg;
      }
    } else if constexpr (std::is_same_v<T, complex_infinity>) {
      ++builder.num_infinities;
    } else {
      // Everything else: Just raise the power by +1.
      const auto [it, was_inserted] = builder.terms.emplace(input_expression, constants::one);
      if (!was_inserted) {
        it->second = it->second + constants::one;
      }
    }
  }

  multiplication_parts& builder;
};

multiplication_parts::multiplication_parts(const multiplication& mul, bool factorize_integers)
    : multiplication_parts(mul.size()) {
  for (const scalar_expr& expr : mul) {
    multiply_term(expr, factorize_integers);
  }
  normalize_coefficients();
}

void multiplication_parts::multiply_term(const scalar_expr& arg, bool factorize_integers) {
  if (factorize_integers) {
    multiply_visitor<true> visitor{*this};
    visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  } else {
    multiply_visitor<false> visitor{*this};
    visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  }
}

void multiplication_parts::normalize_coefficients() {
  for (auto it = terms.begin(); it != terms.end(); ++it) {
    const integer_constant* base = cast_ptr<const integer_constant>(it->first);
    const rational_constant* exponent = cast_ptr<const rational_constant>(it->second);
    // Check if the exponent is now greater than 1, in which case we factorize it into an integer
    // part and a fractional part. The integer part is multiplied onto the rational coefficient.
    if (base && exponent) {
      const auto [integer_part, fractional_part] = factorize_rational_exponent(*exponent);
      // Update the rational coefficient:
      if (integer_part.get_value() >= 0) {
        rational_coeff =
            rational_coeff *
            rational_constant{integer_power(base->get_value(), integer_part.get_value()), 1};
      } else {
        rational_coeff =
            rational_coeff *
            rational_constant{1, integer_power(base->get_value(), -integer_part.get_value())};
      }

      // We changed the exponent on this term, so update it.
      it->second = scalar_expr(fractional_part);
    }
  }

  // Nuke anything w/ a zero exponent.
  for (auto it = terms.begin(); it != terms.end();) {
    if (is_zero(it->second)) {
      it = terms.erase(it);
    } else {
      ++it;
    }
  }
}

scalar_expr multiplication_parts::create_multiplication() const {
  multiplication::container_type args{};

  // TODO: Would be good to front-load this logic so we can early exit before building the map.
  const bool has_zero_coeff =
      rational_coeff.is_zero() || (float_coeff.has_value() && float_coeff->is_zero());
  if (num_infinities > 0 && has_zero_coeff) {
    // Indeterminate: ∞ * 0, applies to any kind of infinity
    return constants::undefined;
  } else if (num_infinities > 0) {
    // z∞ * z∞ -> z∞
    args.push_back(constants::complex_infinity);
  } else if (has_zero_coeff) {
    return constants::zero;
  }

  // Consider any other numerical terms, if we didn't add infinity in.
  if (args.empty()) {
    if (float_coeff.has_value()) {
      const float_constant promoted_rational = static_cast<float_constant>(rational_coeff);
      args.push_back(make_expr<float_constant>(float_coeff.value() * promoted_rational));
    } else if (rational_coeff.is_one()) {
      // Don't insert a useless one in the multiplication.
    } else {
      args.push_back(scalar_expr(rational_coeff));
    }
  }

  // Convert into a vector of powers, and sort into canonical order:
  std::transform(terms.begin(), terms.end(), std::back_inserter(args),
                 [](const auto& pair) { return power::create(pair.first, pair.second); });

  if (std::any_of(args.begin(), args.end(), &is_undefined)) {
    return constants::undefined;
  }

  return maybe_new_mul(std::move(args));
}

// For multiplications, we need to break the expression up.
std::pair<scalar_expr, scalar_expr> split_multiplication(const multiplication& mul,
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
      // Handle multiplication. We do a faster path for a common case (binary mul where first
      // element is numeric).
      const multiplication& mul = x;
      if (mul.size() == 2 &&
          mul[0].is_type<integer_constant, rational_constant, float_constant>()) {
        return std::make_pair(mul[0], mul[1]);
      }
      return split_multiplication(x, expr);
    } else {
      return std::make_pair(constants::one, expr);
    }
  });
}

multiplication_format_parts get_formatting_info(const multiplication& mul) {
  using base_exp = multiplication_format_parts::base_exp;
  multiplication_format_parts result{};

  // Sort into canonical order:
  absl::InlinedVector<scalar_expr, 16> terms{mul.begin(), mul.end()};
  std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
    const auto abe = as_base_and_exp(a);
    const auto bbe = as_base_and_exp(b);
    return order_struct<scalar_expr>{}(abe.first, bbe.first) == relative_order::less_than;
  });

  std::size_t sign_count = 0;
  for (const scalar_expr& expr : terms) {
    // Extract rationals:
    if (const rational_constant* const rational = cast_ptr<const rational_constant>(expr);
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
    } else if (const integer_constant* const integer = cast_ptr<const integer_constant>(expr);
               integer != nullptr) {
      if (integer->get_value() != 1 && integer->get_value() != -1) {
        result.numerator.emplace_back(integer->abs());
      }
      if (integer->get_value() < 0) {
        ++sign_count;
      }
    } else if (const float_constant* const f = cast_ptr<const float_constant>(expr); f != nullptr) {
      result.numerator.emplace_back(f->abs());
      if (f->get_value() < 0) {
        ++sign_count;
      }
    } else {
      // This isn't a numeric value, so break it into base and exponent:
      auto [base, exponent] = as_base_and_exp(expr);
      // Sort into numerator and denominator, depending on sign of the exponent:
      const auto [coeff, _] = as_coeff_and_mul(exponent);
      // See if the exponent seems negative:
      const bool is_negative_exp = is_negative_number(coeff);
      if (is_negative_exp) {
        if (is_negative_one(exponent)) {
          result.denominator.emplace_back(base_exp{std::move(base), constants::one});
        } else {
          // Flip the sign and create a new power.
          result.denominator.emplace_back(base_exp{std::move(base), -exponent});
        }
      } else {
        result.numerator.emplace_back(base_exp{std::move(base), std::move(exponent)});
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
