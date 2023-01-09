// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>
#include <unordered_map>

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "hashing.h"
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

// Visitor that multiplies integers/rationals by the stored value.
// Integers are promoted to rationals.
struct RationalMultiplier {
  using ReturnType = void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  void Apply(const Rational& r) { result = result * r; }
  void Apply(const Integer& i) { result = result * static_cast<Rational>(i); }

  Rational result{1, 1};
};

// Multiply integer + rational constants.
// Removes them from the vector during processing.
static Rational MultiplyIntegersAndRationals(std::vector<Expr>& input) {
  // Traverse the input w/ the accumulator. At the end it will contain the
  // product of all the integers and rationals.
  RationalMultiplier accumulator{};
  const auto new_end = std::remove_if(input.begin(), input.end(), [&accumulator](const Expr& expr) {
    return VisitStruct(expr, accumulator).has_value();
  });
  input.erase(new_end, input.end());
  return accumulator.result;
}

static std::optional<Float> MultiplyFloats(std::vector<Expr>& input) {
  std::optional<Float> product{};
  const auto new_end = std::remove_if(input.begin(), input.end(), [&product](const Expr& expr) {
    return VisitLambda(expr,
                       [&](const Float& f) {
                         if (!product) {
                           product = f;
                         } else {
                           product = *product * f;
                         }
                       })
        .has_value();
  });
  input.erase(new_end, input.end());
  return product;
}

Multiplication::Multiplication(std::vector<Expr> args) : NAryOp(std::move(args)) {}

struct IsNegativeLikeVisitor {
  using ReturnType = bool;
  constexpr static VisitorPolicy Policy = VisitorPolicy::NoError;

  // Numerics < 0 are all negative.
  bool Apply(const Integer& num) const { return num.GetValue() < 0; }
  bool Apply(const Float& f) const { return f.GetValue() < 0; }
  bool Apply(const Rational& r) const { return r.Numerator() < 0; }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool Apply(const Multiplication& m) const {
    std::size_t count = 0;
    for (const Expr& expr : m.Args()) {
      if (VisitStruct(expr, IsNegativeLikeVisitor{}).value_or(false)) {
        ++count;
      }
    }
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }
};

std::pair<Expr, Expr> Multiplication::SplitByExponent() const {
  std::vector<Expr> numerator{};
  std::vector<Expr> denominator{};
  for (const Expr& expr : args_) {
    // Pull the base and exponent:
    auto [base, exponent] = AsBaseAndExponent(expr);
    // Sort into numerator and denominator, depending on sign of the exponent:
    const auto [coeff, _] = AsCoefficientAndMultiplicand(exponent);
    // See if the exponent seems negative:
    const bool is_negative_exp = VisitStruct(coeff, IsNegativeLikeVisitor{}).value_or(false);
    if (is_negative_exp) {
      if (coeff.IsIdenticalTo(Constants::NegativeOne)) {
        denominator.push_back(std::move(base));
      } else {
        // Flip the sign and create a new power.
        denominator.push_back(Power::Create(std::move(base), -exponent));
      }
    } else {
      numerator.push_back(expr);
    }
  }

  Expr num = MaybeNewMul(std::move(numerator));
  Expr den = MaybeNewMul(std::move(denominator));
  return std::make_pair(std::move(num), std::move(den));
}

Expr Multiplication::FromOperands(const std::vector<Expr>& args) {
  ASSERT(!args.empty());
  if (args.size() < 2) {
    return args.front();
  }

  // Check for zeros:
  // TODO: This is not valid if there are divisions by zero...
  // We need an 'undefined' type.
  const bool contains_zeros = std::any_of(args.begin(), args.end(), &IsZero);
  if (contains_zeros) {
    return Constants::Zero;
  }

  // Check if any of the operands are multiplications:
  std::vector<Expr> unpacked_args;
  unpacked_args.reserve(args.size());
  for (const Expr& arg : args) {
    if (const Multiplication* const mul = TryCast<Multiplication>(arg)) {
      // Multiplications must be flattened:
      const std::vector<Expr>& mul_args = mul->Args();
      unpacked_args.insert(unpacked_args.end(), mul_args.begin(), mul_args.end());
    } else {
      unpacked_args.push_back(arg);
    }
  }

  // Now canonicalize the arguments:
  return CanonicalizeArguments(unpacked_args);
}

struct NormalizeExponentVisitor {
  using ReturnType = Expr;

  explicit NormalizeExponentVisitor(const Rational& coeff) : rational_coeff(coeff) {}

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

  Rational rational_coeff;
};

Expr Multiplication::CanonicalizeArguments(std::vector<Expr>& args) {
  // Extract and multiply constants together:
  const Rational rational_term = MultiplyIntegersAndRationals(args);
  const std::optional<Float> float_term = MultiplyFloats(args);

  // Create a map from base -> power
  // TODO: Try the abseil map container.
  std::unordered_map<Expr, Expr, HashObject, ExprEquality> map{};
  map.reserve(args.size());
  for (const Expr& expr : args) {
    auto [base, exponent] = AsBaseAndExponent(expr);
    // Try to insert. If it already exists, replace the exponent with the sum of exponents:
    const auto [it, was_inserted] = map.emplace(std::move(base), exponent);
    if (!was_inserted) {
      it->second = Addition::FromTwoOperands(it->second, exponent);
    }
  }

  // Now normalize any powers of integers:
  NormalizeExponentVisitor normalize_visitor{rational_term};
  for (auto it = map.begin(); it != map.end(); ++it) {
    std::optional<Expr> updated_exponent =
        VisitBinaryStruct(it->first, it->second, normalize_visitor);
    if (updated_exponent) {
      // We changed the exponent on this term, so update it.
      it->second = std::move(*updated_exponent);
    }
  }

  // Nuke anything w/ a zero exponent.
  for (auto it = map.begin(); it != map.end();) {
    if (IsZero(it->second)) {
      it = map.erase(it);
    } else {
      ++it;
    }
  }

  // Create the result:
  args.clear();
  if (float_term) {
    args.push_back(MakeExpr<Float>(float_term.value() * static_cast<Float>(rational_term)));
  } else if (rational_term.IsOne()) {
    // Don't insert a useless one in the multiplication.
  } else {
    args.push_back(Rational::Create(rational_term));
  }

  // Sort into canonical order:
  // TODO: A cheaper way of doing this...
  std::vector<std::pair<Expr, Expr>> powers;
  powers.reserve(args.size());
  std::copy(map.begin(), map.end(), std::back_inserter(powers));

  std::sort(powers.begin(), powers.end(), [](const auto& a, const auto& b) {
    const std::optional<OrderVisitor::RelativeOrder> order =
        VisitBinaryStruct(a.first, b.first, OrderVisitor{});
    ASSERT(order);
    return order.value() == OrderVisitor::RelativeOrder::LessThan;
  });

  // Insert the rest
  for (auto [base, exponent] : powers) {
    args.push_back(Power::Create(base, exponent));
  }

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

}  // namespace math
