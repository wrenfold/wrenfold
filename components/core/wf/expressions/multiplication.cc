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
  WF_ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  if (any_of(args, &is_undefined)) {
    return constants::undefined;
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

  // Now canonicalize the arguments:
  multiplication_parts builder{args.size()};
  for (const scalar_expr& term : args) {
    builder.multiply_term(term);
  }
  builder.normalize_coefficients();
  return builder.create_multiplication();
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

struct multiply_numeric_constants {
  // Promote integer -> rational.
  template <typename A, typename B,
            typename = enable_if_contains_type_t<A, integer_constant, rational_constant>,
            typename = enable_if_contains_type_t<B, integer_constant, rational_constant>>
  constexpr rational_constant operator()(const A a, const B b) const {
    return static_cast<rational_constant>(a) * static_cast<rational_constant>(b);
  }

  // Promote to float:
  template <typename A, typename = enable_if_contains_type_t<A, float_constant, integer_constant,
                                                             rational_constant>>
  constexpr float_constant operator()(const A a, const float_constant b) const {
    return static_cast<float_constant>(a) * b;
  }

  // Promote to float:
  template <typename B,
            typename = enable_if_contains_type_t<B, integer_constant, rational_constant>>
  constexpr float_constant operator()(const float_constant a, const B b) const {
    return a * static_cast<float_constant>(b);
  }

  template <typename T>
  constexpr multiplication_parts::numeric_constant operator()(
      const multiplication_parts::numeric_constant& a, const T b) const {
    return std::visit(
        [b](auto a_concrete) constexpr -> multiplication_parts::numeric_constant {
          return multiply_numeric_constants{}(a_concrete, b);
        },
        a);
  }
};

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

  void insert_power(const scalar_expr& base, const scalar_expr& exponent) {
    if (const auto [it, was_inserted] = builder.terms.emplace(base, exponent); !was_inserted) {
      scalar_expr updated_exp = it->second + exponent;
      if (const std::optional<scalar_expr> simplified = pow_maybe_simplify(it->first, updated_exp);
          simplified.has_value()) {
        // Thw power now simplifies to something else, so erase this from the terms and visit the
        // simplfied object.
        builder.terms.erase(it);
        visit(*simplified, *this);
      } else {
        it->second = std::move(updated_exp);
      }
    }
  }

  template <typename T>
  void operator()(const T& arg, const scalar_expr& input_expression) {
    if constexpr (std::is_same_v<T, multiplication>) {
      for (const scalar_expr& expr : arg) {
        // Recursively add multiplications:
        visit(expr, *this);
      }
    } else if constexpr (std::is_same_v<T, power>) {
      insert_power(arg.base(), arg.exponent());
    } else if constexpr (std::is_same_v<T, integer_constant>) {
      if constexpr (FactorizeIntegers) {
        // Factorize integers into primes:
        const auto factors = compute_prime_factors(arg.value());
        insert_integer_factors(factors, true);
      } else {
        // Promote integers to rationals and multiply them onto `rational_coeff`.
        builder.numeric_coeff = multiply_numeric_constants{}(builder.numeric_coeff, arg);
      }
    } else if constexpr (std::is_same_v<T, rational_constant>) {
      if constexpr (FactorizeIntegers) {
        const auto num_factors = compute_prime_factors(arg.numerator());
        const auto den_factors = compute_prime_factors(arg.denominator());
        insert_integer_factors(num_factors, true);
        insert_integer_factors(den_factors, false);
      } else {
        builder.numeric_coeff = multiply_numeric_constants{}(builder.numeric_coeff, arg);
      }
    } else if constexpr (std::is_same_v<T, float_constant>) {
      builder.numeric_coeff = multiply_numeric_constants{}(builder.numeric_coeff, arg);
    } else if constexpr (std::is_same_v<T, complex_infinity>) {
      ++builder.num_infinities;
    } else {
      // Everything else: Just raise the power by +1.
      insert_power(input_expression, constants::one);
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
  // Nuke anything w/ a zero exponent.
  map_erase_if(terms, [](const auto& pair) { return is_zero(pair.second); });
}

scalar_expr multiplication_parts::create_multiplication() const {
  multiplication::container_type args{};

  // TODO: Would be good to front-load this logic so we can early exit before building the map.
  if (has_complex_infinity() && has_zero_numeric_coefficient()) {
    // Indeterminate: ∞ * 0, applies to any kind of infinity
    return constants::undefined;
  } else if (has_complex_infinity()) {
    // z∞ * z∞ -> z∞
    args.push_back(constants::complex_infinity);
  } else if (has_zero_numeric_coefficient()) {
    return constants::zero;
  }

  multiplication_parts::numeric_constant numeric_term = numeric_coeff;

  // Convert into a vector of powers, and sort into canonical order:
  for (const auto& [base, exp] : terms) {
    auto pow = power::create(base, exp);

    // The power may have produced a numerical coefficient:
    auto [coeff, mul] = as_coeff_and_mul(pow);
    if (!is_one(coeff)) {
      // If there is a non-unit coefficient resulting from the power, multiply it onto
      // `numeric_coeff`.
      visit(coeff, [&](const auto& numeric) {
        using T = std::decay_t<decltype(numeric)>;
        if constexpr (type_list_contains_v<T, integer_constant, rational_constant,
                                           float_constant>) {
          numeric_term = multiply_numeric_constants{}(numeric_term, numeric);
        } else {
          // TODO: as_coeff_and_mul should probably just return `numeric_constant`.
          WF_ASSERT_ALWAYS("Unexpected type: {}", T::name_str);
        }
      });
    }
    if (!is_one(mul)) {
      args.push_back(std::move(mul));
    }
  }

  scalar_expr numerical_coeff_expr = overloaded_visit(
      numeric_term, [](const float_constant f) { return scalar_expr(f.value()); },
      [](const rational_constant r) { return scalar_expr(r); });

  if (args.size() == 1) {
    // Other than the numeric value, we only have one expression.
    // If this term is an addition, distribute the numerical value over the addition:
    if (const addition* add = get_if<const addition>(args[0]);
        add != nullptr && !is_one(numerical_coeff_expr)) {
      return multiply_into_addition(*add, numerical_coeff_expr);
    }
  }

  if (!is_one(numerical_coeff_expr) && !has_complex_infinity()) {
    args.push_back(std::move(numerical_coeff_expr));
  }
  if (any_of(args, &is_undefined)) {
    return constants::undefined;
  }
  return maybe_new_mul(std::move(args));
}

bool multiplication_parts::has_zero_numeric_coefficient() const {
  return std::visit([](const auto& x) { return x.is_zero(); }, numeric_coeff);
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
