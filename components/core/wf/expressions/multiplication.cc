// Copyright 2022 Gareth Cross
#include "wf/expressions/multiplication.h"

#include <algorithm>

#include "wf/common_visitors.h"
#include "wf/expressions/all_expressions.h"
#include "wf/integer_utils.h"
#include "wf/visitor_impl.h"

namespace math {

inline Expr maybe_new_mul(Multiplication::ContainerType&& terms) {
  if (terms.empty()) {
    return Constants::One;
  } else if (terms.size() == 1) {
    return std::move(terms.front());
  } else {
    return make_expr<Multiplication>(std::move(terms));
  }
}

static inline Expr multiply_into_addition(const Addition& add, const Expr& numerical_constant) {
  Addition::ContainerType add_args{};
  add_args.reserve(add.arity());
  std::transform(
      add.begin(), add.end(), std::back_inserter(add_args),
      [&numerical_constant](const Expr& add_term) { return add_term * numerical_constant; });
  return Addition::from_operands(add_args);  // TODO: make this a move!
}

Expr Multiplication::from_operands(absl::Span<const Expr> args) {
  WF_ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  if (std::any_of(args.begin(), args.end(), &is_undefined)) {
    return Constants::Undefined;
  }

  // TODO: this simplification doesn't always work because there might be multiple
  // integer/rational/float terms.
  if (args.size() == 2) {
    if (const Addition* add = cast_ptr<Addition>(args[0]);
        add && args[1].is_type<Integer, Rational, Float>()) {
      return multiply_into_addition(*add, args[1]);
    } else if (add = cast_ptr<Addition>(args[1]);
               add && args[0].is_type<Integer, Float, Rational>()) {
      return multiply_into_addition(*add, args[0]);
    }
  }

  // Now canonicalize the arguments:
  MultiplicationParts builder{args.size()};
  for (const Expr& term : args) {
    builder.multiply_term(term);
  }
  builder.normalize_coefficients();
  return builder.create_multiplication();
}

template <bool FactorizeIntegers>
struct MultiplyVisitor {
  explicit MultiplyVisitor(MultiplicationParts& builder) : builder(builder) {}

  void insert_integer_factors(const std::vector<PrimeFactor>& factors, bool positive) {
    for (const PrimeFactor& factor : factors) {
      Expr base = Integer::create(factor.base);
      Expr exponent = Integer::create(factor.exponent);
      const auto [it, was_inserted] = builder.terms.emplace(std::move(base), exponent);
      if (!was_inserted) {
        if (positive) {
          it->second = it->second + exponent;
        } else {
          it->second = it->second - exponent;
        }
      }
    }
  }

  template <typename T>
  void operator()(const T& arg, const Expr& input_expression) {
    if constexpr (std::is_same_v<T, Multiplication>) {
      for (const Expr& expr : arg) {
        // Recursively add multiplications:
        visit(expr, [this, &expr](const auto& x) { this->operator()(x, expr); });
      }
    } else if constexpr (std::is_same_v<T, Power>) {
      const Power& arg_pow = arg;
      // Try to insert. If it already exists, replace the exponent with the sum of exponents:
      const auto [it, was_inserted] = builder.terms.emplace(arg_pow.base(), arg_pow.exponent());
      if (!was_inserted) {
        it->second = it->second + arg_pow.exponent();
      }
    } else if constexpr (std::is_same_v<T, Integer>) {
      if constexpr (FactorizeIntegers) {
        // Factorize integers into primes:
        const auto factors = compute_prime_factors(arg.get_value());
        insert_integer_factors(factors, true);
      } else {
        // Promote integers to rationals and multiply them onto `rational_coeff`.
        builder.rational_coeff = builder.rational_coeff * static_cast<Rational>(arg);
      }
    } else if constexpr (std::is_same_v<T, Rational>) {
      if constexpr (FactorizeIntegers) {
        const auto num_factors = compute_prime_factors(arg.numerator());
        const auto den_factors = compute_prime_factors(arg.denominator());
        insert_integer_factors(num_factors, true);
        insert_integer_factors(den_factors, false);
      } else {
        builder.rational_coeff = builder.rational_coeff * arg;
      }
    } else if constexpr (std::is_same_v<T, Float>) {
      if (!builder.float_coeff.has_value()) {
        builder.float_coeff = arg;
      } else {
        builder.float_coeff = (*builder.float_coeff) * arg;
      }
    } else if constexpr (std::is_same_v<T, Infinity>) {
      ++builder.num_infinities;
    } else {
      // Everything else: Just raise the power by +1.
      const auto [it, was_inserted] = builder.terms.emplace(input_expression, Constants::One);
      if (!was_inserted) {
        it->second = it->second + Constants::One;
      }
    }
  }

  MultiplicationParts& builder;
};

MultiplicationParts::MultiplicationParts(const Multiplication& mul, bool factorize_integers)
    : MultiplicationParts(mul.arity()) {
  for (const Expr& expr : mul) {
    multiply_term(expr, factorize_integers);
  }
  normalize_coefficients();
}

void MultiplicationParts::multiply_term(const Expr& arg, bool factorize_integers) {
  if (factorize_integers) {
    MultiplyVisitor<true> visitor{*this};
    visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  } else {
    MultiplyVisitor<false> visitor{*this};
    visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  }
}

void MultiplicationParts::normalize_coefficients() {
  for (auto it = terms.begin(); it != terms.end(); ++it) {
    const Integer* base = cast_ptr<Integer>(it->first);
    const Rational* exponent = cast_ptr<Rational>(it->second);
    // Check if the exponent is now greater than 1, in which case we factorize it into an integer
    // part and a fractional part. The integer part is multiplied onto the rational coefficient.
    if (base && exponent) {
      const auto [integer_part, fractional_part] = factorize_rational_exponent(*exponent);
      // Update the rational coefficient:
      if (integer_part.get_value() >= 0) {
        rational_coeff = rational_coeff *
                         Rational{integer_power(base->get_value(), integer_part.get_value()), 1};
      } else {
        rational_coeff = rational_coeff *
                         Rational{1, integer_power(base->get_value(), -integer_part.get_value())};
      }

      // We changed the exponent on this term, so update it.
      it->second = Rational::create(fractional_part);
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

Expr MultiplicationParts::create_multiplication() const {
  Multiplication::ContainerType args{};

  // TODO: Would be good to front-load this logic so we can early exit before building the map.
  const bool has_zero_coeff =
      rational_coeff.is_zero() || (float_coeff.has_value() && float_coeff->is_zero());
  if (num_infinities > 0 && has_zero_coeff) {
    // Indeterminate: ∞ * 0, applies to any kind of infinity
    return Constants::Undefined;
  } else if (num_infinities > 0) {
    // z∞ * z∞ -> z∞
    args.push_back(Constants::ComplexInfinity);
  } else if (has_zero_coeff) {
    return Constants::Zero;
  }

  // Consider any other numerical terms, if we didn't add infinity in.
  if (args.empty()) {
    if (float_coeff.has_value()) {
      const Float promoted_rational = static_cast<Float>(rational_coeff);
      args.push_back(make_expr<Float>(float_coeff.value() * promoted_rational));
    } else if (rational_coeff.is_one()) {
      // Don't insert a useless one in the multiplication.
    } else {
      args.push_back(Rational::create(rational_coeff));
    }
  }

  // Convert into a vector of powers, and sort into canonical order:
  std::transform(terms.begin(), terms.end(), std::back_inserter(args),
                 [](const auto& pair) { return Power::create(pair.first, pair.second); });

  if (std::any_of(args.begin(), args.end(), &is_undefined)) {
    return Constants::Undefined;
  }

  return maybe_new_mul(std::move(args));
}

// For multiplications, we need to break the expression up.
std::pair<Expr, Expr> split_multiplication(const Multiplication& mul, const Expr& mul_abstract) {
  Multiplication::ContainerType numerics{};
  Multiplication::ContainerType remainder{};
  for (const Expr& expr : mul) {
    if (is_numeric(expr)) {
      numerics.push_back(expr);
    } else {
      remainder.push_back(expr);
    }
  }
  if (numerics.empty()) {
    // No point making a new multiplication:
    return std::make_pair(Constants::One, mul_abstract);
  }
  auto coeff = maybe_new_mul(std::move(numerics));
  auto multiplicand = maybe_new_mul(std::move(remainder));
  return std::make_pair(std::move(coeff), std::move(multiplicand));
}

std::pair<Expr, Expr> as_coeff_and_mul(const Expr& expr) {
  return visit(expr, [&expr](const auto& x) -> std::pair<Expr, Expr> {
    using T = std::decay_t<decltype(x)>;
    if constexpr (type_list_contains_type_v<T, Integer, Rational, Float>) {
      // Numerical values are always the coefficient:
      return std::make_pair(expr, Constants::One);
    } else if constexpr (std::is_same_v<T, Multiplication>) {
      // Handle multiplication. We do a faster path for a common case (binary mul where first
      // element is numeric).
      const Multiplication& mul = x;
      if (mul.arity() == 2 && mul[0].is_type<Integer, Rational, Float>()) {
        return std::make_pair(mul[0], mul[1]);
      }
      return split_multiplication(x, expr);
    } else {
      return std::make_pair(Constants::One, expr);
    }
  });
}

MultiplicationFormattingInfo get_formatting_info(const Multiplication& mul) {
  using BaseExp = MultiplicationFormattingInfo::BaseExp;
  MultiplicationFormattingInfo result{};

  // Sort into canonical order:
  absl::InlinedVector<Expr, 16> terms{mul.begin(), mul.end()};
  std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
    const auto abe = as_base_and_exp(a);
    const auto bbe = as_base_and_exp(b);
    return expression_order(abe.first, bbe.first) == RelativeOrder::LessThan;
  });

  std::size_t sign_count = 0;
  for (const Expr& expr : terms) {
    // Extract rationals:
    if (const Rational* const rational = cast_ptr<Rational>(expr); rational != nullptr) {
      const auto abs_num = std::abs(rational->numerator());
      if (abs_num != 1) {
        // Don't put redundant ones into the numerator for rationals of the form 1/n.
        result.numerator.emplace_back(Integer{abs_num});
      }
      result.denominator.emplace_back(Integer{rational->denominator()});

      if (rational->numerator() < 0) {
        // If negative, increase the sign count.
        ++sign_count;
      }
    } else if (const Integer* const integer = cast_ptr<Integer>(expr); integer != nullptr) {
      if (integer->get_value() != 1 && integer->get_value() != -1) {
        result.numerator.emplace_back(integer->abs());
      }
      if (integer->get_value() < 0) {
        ++sign_count;
      }
    } else if (const Float* const f = cast_ptr<Float>(expr); f != nullptr) {
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
          result.denominator.emplace_back(BaseExp{std::move(base), Constants::One});
        } else {
          // Flip the sign and create a new power.
          result.denominator.emplace_back(BaseExp{std::move(base), -exponent});
        }
      } else {
        result.numerator.emplace_back(BaseExp{std::move(base), std::move(exponent)});
      }
    }
  }

  result.is_negative = static_cast<bool>(sign_count & 1);  //  Even = positive, Odd = negative
  if (result.numerator.empty()) {
    // If all powers were negative, and we had only a rational, the numerator may be empty:
    result.numerator.emplace_back(Integer{1});
  }
  return result;
}

}  // namespace math
