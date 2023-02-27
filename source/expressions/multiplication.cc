// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "integer_utils.h"
#include "ordering.h"
#include "string_utils.h"
#include "tree_formatter.h"
#include "visitor_impl.h"

namespace math {

inline Expr MaybeNewMul(std::vector<Expr>&& terms) {
  if (terms.empty()) {
    return Constants::One;
  } else if (terms.size() == 1) {
    return terms.front();
  } else {
    return MakeExpr<Multiplication>(std::move(terms));
  }
}

Multiplication::Multiplication(std::vector<Expr> args) : NAryOp(std::move(args)) {}

Expr Multiplication::FromOperands(const std::vector<Expr>& args) {
  ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  if (std::any_of(args.begin(), args.end(), &TryCast<Matrix>)) {
    // TODO: Don't copy here - operate on args directly.
    std::vector<Expr> scalars = args;
    std::optional<Matrix> matrix_product{};  //  Optional because we can't default initialize.
    const auto new_end =
        std::remove_if(scalars.begin(), scalars.end(), [&matrix_product](const Expr& expr) -> bool {
          // This is a matrix, pull it out and chain them together. If dimensions don't work out,
          // we'll throw in the multiplication operator.
          if (const Matrix* m = TryCast<Matrix>(expr); m != nullptr) {
            if (!matrix_product) {
              matrix_product = std::move(*m);
            } else {
              matrix_product = matrix_product.value() * *m;
            }
            return true;
          }
          return false;
        });

    ASSERT(matrix_product.has_value(), "Must have been at least one matrix");
    scalars.erase(new_end, scalars.end());

    Matrix result = std::move(*matrix_product);
    if (!scalars.empty()) {
      // If there were any scalar terms, multiply them into the matrix now:
      result.MultiplyByScalarInPlace(FromOperands(scalars));
    }
    if (result.NumRows() == 1 && result.NumCols() == 1) {
      // Discard the matrix dimension, and return a scalar.
      return result[0];
    }
    return MakeExpr<Matrix>(std::move(result));
  }

  // Check for zeros:
  // TODO: This is not valid if there are divisions by zero...
  // We need an 'undefined' type.
  const bool contains_zeros = std::any_of(args.begin(), args.end(), &IsZero);
  if (contains_zeros) {
    return Constants::Zero;
  }

  // Now canonicalize the arguments:
  // TODO: Get rid of this copy.
  std::vector<Expr> mutable_args = args;
  return CanonicalizeArguments(mutable_args);
}

Expr Multiplication::CanonicalizeArguments(std::vector<Expr>& args) {
  MultiplicationBuilder builder{args.size()};
  for (const Expr& expr : args) {
    builder.Multiply(expr);
  }
  builder.Normalize();
  return builder.CreateMultiplication(std::move(args));
}

struct MultiplyVisitor {
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = void;

  MultiplyVisitor(const Expr& input_expression, MultiplicationBuilder& builder)
      : input_expression(input_expression), builder(builder) {}

  template <typename T>
  void Apply(const T& arg) {
    if constexpr (std::is_same_v<T, Multiplication>) {
      for (const Expr& expr : arg) {
        // Recursively add multiplications:
        VisitStruct(expr, MultiplyVisitor{expr, builder});
      }
    } else if constexpr (std::is_same_v<T, Power>) {
      // Try to insert. If it already exists, replace the exponent with the sum of exponents:
      const auto [it, was_inserted] = builder.terms.emplace(arg.Base(), arg.Exponent());
      if (!was_inserted) {
        it->second = Addition::FromTwoOperands(it->second, arg.Exponent());
      }
    } else if constexpr (std::is_same_v<T, Integer> || std::is_same_v<T, Rational>) {
      // Promote integers to rationals and multiply them onto `rational_coeff`.
      builder.rational_coeff = builder.rational_coeff * static_cast<Rational>(arg);
    } else if constexpr (std::is_same_v<T, Float>) {
      if (!builder.float_coeff.has_value()) {
        builder.float_coeff = arg;
      } else {
        builder.float_coeff = (*builder.float_coeff) * arg;
      }
    } else if constexpr (std::is_same_v<T, Matrix>) {
      throw TypeError(
          "Cannot multiply a matrix into a scalar multiplication expression. Arg type = {}",
          arg.TypeName());
    } else {
      // Everything else: Just raise the power by +1.
      const auto [it, was_inserted] = builder.terms.emplace(input_expression, Constants::One);
      if (!was_inserted) {
        it->second = Addition::FromTwoOperands(it->second, Constants::One);
      }
    }
  }

  const Expr& input_expression;
  MultiplicationBuilder& builder;
};

struct NormalizeExponentVisitor {
  using ReturnType = Expr;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  explicit NormalizeExponentVisitor(Rational& coeff) : rational_coeff(coeff) {}

  // Check if the exponent is now greater than 1, in which case we factorize it into an integer part
  // and a fractional part. The integer part is multiplied onto the rational coefficient.
  Expr Apply(const Integer& base, const Rational& exponent) {
    const auto [integer_part, fractional_part] = FactorizeRationalExponent(exponent);
    // Update the rational coefficient:
    if (integer_part.GetValue() >= 0) {
      rational_coeff = rational_coeff * Rational{Pow(base.GetValue(), integer_part.GetValue()), 1};
    } else {
      rational_coeff = rational_coeff * Rational{1, Pow(base.GetValue(), -integer_part.GetValue())};
    }
    return Rational::Create(fractional_part);
  }

  Rational& rational_coeff;
};

void MultiplicationBuilder::Multiply(const Expr& arg) {
  VisitStruct(arg, MultiplyVisitor{arg, *this});
}

void MultiplicationBuilder::Normalize() {
  NormalizeExponentVisitor normalize_visitor{rational_coeff};
  for (auto it = terms.begin(); it != terms.end(); ++it) {
    std::optional<Expr> updated_exponent =
        VisitBinaryStruct(it->first, it->second, normalize_visitor);
    if (updated_exponent.has_value()) {
      // We changed the exponent on this term, so update it.
      it->second = std::move(*updated_exponent);
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

Expr MultiplicationBuilder::CreateMultiplication(std::vector<Expr>&& args) const {
  // Create the result:
  args.clear();
  args.reserve(terms.size() + 2);
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

  std::sort(args.begin(), args.end(), [](const Expr& a, const Expr& b) {
    return VisitBinaryStruct(a, b, OrderVisitor{}) == OrderVisitor::RelativeOrder::LessThan;
  });
  return MaybeNewMul(std::move(args));
}

struct AsCoeffAndMultiplicandVisitor {
  using ReturnType = std::pair<Expr, Expr>;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  // For multiplications, we need to break the expression up.
  ReturnType Apply(const Multiplication& mul) const {
    // TODO: Small vector.
    std::vector<Expr> numerics{};
    std::vector<Expr> remainder{};
    for (const Expr& expr : mul.Args()) {
      if (IsNumeric(expr)) {
        numerics.push_back(expr);
      } else {
        remainder.push_back(expr);
      }
    }
    if (numerics.empty()) {
      // No point making a new multiplication:
      return std::make_pair(Constants::One, arg_);
    }
    auto coeff = MaybeNewMul(std::move(numerics));
    auto multiplicand = MaybeNewMul(std::move(remainder));
    return std::make_pair(std::move(coeff), std::move(multiplicand));
  }

  // If the input type is a numeric, return the numeric as a coefficient for multiplicand of one.
  template <typename T>
  std::enable_if_t<ContainsTypeHelper<T, Integer, Rational, Float>, ReturnType> Apply(
      const T&) const {
    return std::make_pair(arg_, Constants::One);
  };

  // Construct with a reference to the input argument so that we can
  // return it directly when applicable.
  explicit AsCoeffAndMultiplicandVisitor(const Expr& arg) : arg_(arg) {}

  const Expr& arg_;
};

std::pair<Expr, Expr> AsCoefficientAndMultiplicand(const Expr& expr) {
  std::optional<std::pair<Expr, Expr>> result =
      VisitStruct(expr, AsCoeffAndMultiplicandVisitor{expr});
  if (result.has_value()) {
    return *result;
  }
  return std::make_pair(Constants::One, expr);
}

MultiplicationFormattingInfo GetFormattingInfo(const Multiplication& mul) {
  using BaseExp = MultiplicationFormattingInfo::BaseExp;
  MultiplicationFormattingInfo result{};

  std::size_t sign_count = 0;
  for (const Expr& expr : mul) {
    // Extract rationals:
    if (const Rational* const rational = TryCast<Rational>(expr); rational != nullptr) {
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
    } else if (const Integer* const integer = TryCast<Integer>(expr); integer != nullptr) {
      if (integer->GetValue() != 1 && integer->GetValue() != -1) {
        result.numerator.emplace_back(integer->Abs());
      }
      if (integer->GetValue() < 0) {
        ++sign_count;
      }
    } else if (const Float* const f = TryCast<Float>(expr); f != nullptr) {
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
