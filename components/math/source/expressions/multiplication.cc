// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "string_utils.h"
#include "visitor_impl.h"

namespace math {

inline Expr MaybeNewMul(Multiplication::ContainerType&& terms) {
  if (terms.empty()) {
    return Constants::One;
  } else if (terms.size() == 1) {
    return std::move(terms.front());
  } else {
    return MakeExpr<Multiplication>(std::move(terms));
  }
}

inline Expr MultiplyMatrixOperands(const absl::Span<const Expr>& args) {
  Multiplication::ContainerType scalars;
  scalars.reserve(args.size() / 2);

  std::optional<Matrix> matrix_product{};  //  Optional because we can't default initialize.
  for (const Expr& term : args) {
    if (const Matrix* m = CastPtr<Matrix>(term); m != nullptr) {
      if (!matrix_product) {
        // We need to copy:
        matrix_product = *m;
      } else {
        // TODO: Avoid copies if dimensions don't change.
        matrix_product = matrix_product.value() * *m;
      }
    } else {
      scalars.push_back(term);
    }
  }

  ASSERT(matrix_product.has_value(), "Must have been at least one matrix");

  Matrix result = std::move(*matrix_product);
  if (!scalars.empty()) {
    // If there were any scalar terms, multiply them into the matrix now:
    result.MultiplyByScalarInPlace(Multiplication::FromOperands(scalars));
  }
  if (result.NumRows() == 1 && result.NumCols() == 1) {
    // Discard the matrix dimension, and return a scalar.
    // TODO: Remove this behavior!
    return result[0];
  }
  return MakeExpr<Matrix>(std::move(result));
}

Expr Multiplication::FromOperands(absl::Span<const Expr> args) {
  ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  const bool any_matrices =
      std::any_of(args.begin(), args.end(), [](const Expr& x) { return x.Is<Matrix>(); });
  if (any_matrices) {
    return MultiplyMatrixOperands(args);
  }

  // Check for zeros:
  // TODO: This is not valid if there are divisions by zero...
  // We need an 'undefined' type.
  const bool contains_zeros = std::any_of(args.begin(), args.end(), &IsZero);
  if (contains_zeros) {
    return Constants::Zero;
  }

  // Now canonicalize the arguments:
  MultiplicationParts builder{args.size()};
  for (const Expr& term : args) {
    builder.Multiply(term);
  }
  builder.Normalize();
  return builder.CreateMultiplication();
}

template <bool FactorizeIntegers>
struct MultiplyVisitor {
  using ReturnType = void;

  explicit MultiplyVisitor(MultiplicationParts& builder) : builder(builder) {}

  void InsertIntegerFactors(const std::vector<PrimeFactor>& factors, bool positive) {
    for (const PrimeFactor& factor : factors) {
      Expr base = Integer::Create(factor.base);
      Expr exponent = Integer::Create(factor.exponent);
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
        Visit(expr, [this, &expr](const auto& x) { operator()(x, expr); });
      }
    } else if constexpr (std::is_same_v<T, Power>) {
      const Power& arg_pow = arg;
      // Try to insert. If it already exists, replace the exponent with the sum of exponents:
      const auto [it, was_inserted] = builder.terms.emplace(arg_pow.Base(), arg_pow.Exponent());
      if (!was_inserted) {
        it->second = it->second + arg_pow.Exponent();
      }
    } else if constexpr (std::is_same_v<T, Integer>) {
      if constexpr (FactorizeIntegers) {
        // Factorize integers into primes:
        const auto factors = ComputePrimeFactors(arg.GetValue());
        InsertIntegerFactors(factors, true);
      } else {
        // Promote integers to rationals and multiply them onto `rational_coeff`.
        builder.rational_coeff = builder.rational_coeff * static_cast<Rational>(arg);
      }
    } else if constexpr (std::is_same_v<T, Rational>) {
      if constexpr (FactorizeIntegers) {
        const auto num_factors = ComputePrimeFactors(arg.Numerator());
        const auto den_factors = ComputePrimeFactors(arg.Denominator());
        InsertIntegerFactors(num_factors, true);
        InsertIntegerFactors(den_factors, false);
      } else {
        builder.rational_coeff = builder.rational_coeff * arg;
      }
    } else if constexpr (std::is_same_v<T, Float>) {
      if (!builder.float_coeff.has_value()) {
        builder.float_coeff = arg;
      } else {
        builder.float_coeff = (*builder.float_coeff) * arg;
      }
    } else if constexpr (std::is_same_v<T, Matrix>) {
      throw TypeError(
          "Cannot multiply a matrix into a scalar multiplication expression. Arg type = {}",
          T::NameStr);
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
    : MultiplicationParts(mul.Arity()) {
  for (const Expr& expr : mul) {
    Multiply(expr, factorize_integers);
  }
  Normalize();
}

void MultiplicationParts::Multiply(const Expr& arg, bool factorize_integers) {
  if (factorize_integers) {
    MultiplyVisitor<true> visitor{*this};
    Visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  } else {
    MultiplyVisitor<false> visitor{*this};
    Visit(arg, [&visitor, &arg](const auto& x) { visitor(x, arg); });
  }
}

void MultiplicationParts::Normalize() {
  for (auto it = terms.begin(); it != terms.end(); ++it) {
    const Integer* base = CastPtr<Integer>(it->first);
    const Rational* exponent = CastPtr<Rational>(it->second);
    // Check if the exponent is now greater than 1, in which case we factorize it into an integer
    // part and a fractional part. The integer part is multiplied onto the rational coefficient.
    if (base && exponent) {
      const auto [integer_part, fractional_part] = FactorizeRationalExponent(*exponent);
      // Update the rational coefficient:
      if (integer_part.GetValue() >= 0) {
        rational_coeff =
            rational_coeff * Rational{Pow(base->GetValue(), integer_part.GetValue()), 1};
      } else {
        rational_coeff =
            rational_coeff * Rational{1, Pow(base->GetValue(), -integer_part.GetValue())};
      }

      // We changed the exponent on this term, so update it.
      it->second = Rational::Create(fractional_part);
    }
  }

  // Nuke anything w/ a zero exponent.
  for (auto it = terms.begin(); it != terms.end();) {
    if (IsZero(it->second)) {
      it = terms.erase(it);
    } else {
      ++it;
    }
  }
}

Expr MultiplicationParts::CreateMultiplication() const {
  // Create the result:
  Multiplication::ContainerType args{};
  args.reserve(terms.size() + 1);
  if (float_coeff.has_value()) {
    const Float promoted_rational = static_cast<Float>(rational_coeff);
    args.push_back(MakeExpr<Float>(float_coeff.value() * promoted_rational));
  } else if (rational_coeff.IsOne()) {
    // Don't insert a useless one in the multiplication.
  } else {
    args.push_back(Rational::Create(rational_coeff));
  }

  // Convert into a vector of powers, and sort into canonical order:
  std::transform(terms.begin(), terms.end(), std::back_inserter(args),
                 [](const auto& pair) { return Power::Create(pair.first, pair.second); });

  return MaybeNewMul(std::move(args));
}

// For multiplications, we need to break the expression up.
inline std::pair<Expr, Expr> SplitMultiplication(const Expr& input, const Multiplication& mul) {
  Multiplication::ContainerType numerics{};
  Multiplication::ContainerType remainder{};
  for (const Expr& expr : mul) {
    if (IsNumeric(expr)) {
      numerics.push_back(expr);
    } else {
      remainder.push_back(expr);
    }
  }
  if (numerics.empty()) {
    // No point making a new multiplication:
    return std::make_pair(Constants::One, input);
  }
  auto coeff = MaybeNewMul(std::move(numerics));
  auto multiplicand = MaybeNewMul(std::move(remainder));
  return std::make_pair(std::move(coeff), std::move(multiplicand));
}

std::pair<Expr, Expr> AsCoefficientAndMultiplicand(const Expr& expr) {
  return Visit(expr, [&expr](const auto& x) -> std::pair<Expr, Expr> {
    using T = std::decay_t<decltype(x)>;
    if constexpr (ContainsTypeHelper<T, Integer, Rational, Float>) {
      // Numerical values are always the coefficient:
      return std::make_pair(expr, Constants::One);
    } else if constexpr (std::is_same_v<T, Multiplication>) {
      // Handle multiplication. We do a faster path for a common case (binary mul where first
      // element is numeric).
      const Multiplication& mul = x;
      if (mul.Arity() == 2 && mul[0].Is<Integer, Rational, Float>()) {
        return std::make_pair(mul[0], mul[1]);
      }
      return SplitMultiplication(expr, x);
    } else {
      return std::make_pair(Constants::One, expr);
    }
  });
}

MultiplicationFormattingInfo GetFormattingInfo(const Multiplication& mul) {
  using BaseExp = MultiplicationFormattingInfo::BaseExp;
  MultiplicationFormattingInfo result{};

  // Sort into canonical order:
  absl::InlinedVector<Expr, 16> terms{mul.begin(), mul.end()};
  std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
    const auto abe = AsBaseAndExponent(a);
    const auto bbe = AsBaseAndExponent(b);
    return ExpressionOrder(abe.first, bbe.first) == RelativeOrder::LessThan;
  });

  std::size_t sign_count = 0;
  for (const Expr& expr : terms) {
    // Extract rationals:
    if (const Rational* const rational = CastPtr<Rational>(expr); rational != nullptr) {
      const auto abs_num = std::abs(rational->Numerator());
      if (abs_num != 1) {
        // Don't put redundant ones into the numerator for rationals of the form 1/n.
        result.numerator.emplace_back(Integer{abs_num});
      }
      result.denominator.emplace_back(Integer{rational->Denominator()});

      if (rational->Numerator() < 0) {
        // If negative, increase the sign count.
        ++sign_count;
      }
    } else if (const Integer* const integer = CastPtr<Integer>(expr); integer != nullptr) {
      if (integer->GetValue() != 1 && integer->GetValue() != -1) {
        result.numerator.emplace_back(integer->Abs());
      }
      if (integer->GetValue() < 0) {
        ++sign_count;
      }
    } else if (const Float* const f = CastPtr<Float>(expr); f != nullptr) {
      result.numerator.emplace_back(f->Abs());
      if (f->GetValue() < 0) {
        ++sign_count;
      }
    } else {
      // This isn't a numeric value, so break it into base and exponent:
      auto [base, exponent] = AsBaseAndExponent(expr);
      // Sort into numerator and denominator, depending on sign of the exponent:
      const auto [coeff, _] = AsCoefficientAndMultiplicand(exponent);
      // See if the exponent seems negative:
      const bool is_negative_exp = IsNegativeNumber(coeff);
      if (is_negative_exp) {
        if (IsNegativeOne(exponent)) {
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