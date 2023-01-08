// Copyright 2022 Gareth Cross
#include "expressions/multiplication.h"

#include <algorithm>
#include <unordered_map>

#include "expressions/all_expressions.h"
#include "hashing.h"
#include "integer_utils.h"
#include "string_utils.h"
#include "tree_formatter.h"

namespace math {

// Visitor that multiplies integers/rationals by the stored value.
struct RationalAccumulator {
  using ReturnType = void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  void Apply(const Rational& r) { result = result * r; }
  void Apply(const Integer& i) { result = result * static_cast<Rational>(i); }

  Rational result{1, 1};
};

// Multiply integer + rational constants.
// Removes them from the vector during processing.
static Rational MultiplyIntegersAndRationals(std::vector<Expr>& input) {
  RationalAccumulator accumulator{};
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
//
//// Rule:
////  float -> promote everything to float
////  rational -> promote integers to rationals
////  integer -> leave everything as integers
// static Expr MultiplyScalars(const std::optional<Float>& float_value,
//                             const std::optional<Integer>& int_value) {
//   if (float_value) {
//     Float result = *float_value;
//     if (int_value) {
//       // Promote integers to float.
//       result = result * static_cast<Float>(*int_value);
//     }
//     // TODO: Should we check if the result of multiplication is actually just an integer?
//     return MakeExpr<Float>(result);
//   } else if (int_value) {
//     return Integer::Create(*int_value);
//   }
//   return Constants::One;
// }

Multiplication::Multiplication(std::vector<Expr> args) : NAryOp(std::move(args)) {}

std::pair<Expr, Expr> Multiplication::SplitByExponent() const {
  std::vector<Expr> numerator{};
  std::vector<Expr> denominator{};
  for (const Expr& expr : args_) {
    // Pull the base and exponent:
    auto [base, exponent] = AsBaseAndExponent(expr);
    // Sort into numerator and denominator, depending on sign of the exponent:
    const Expr exponent_coeff = CoefficientVisitor::GetCoefficient(exponent);
    if (IsNegativeNumber(exponent_coeff)) {
      if (exponent_coeff.IsIdenticalTo(Constants::NegativeOne)) {
        denominator.push_back(std::move(base));
      } else {
        denominator.push_back(Power::Create(std::move(base), -exponent));
      }
    } else {
      numerator.push_back(expr);
    }
  }
  auto num =
      numerator.empty() ? Constants::One : Multiplication::FromOperands(std::move(numerator));
  auto den =
      denominator.empty() ? Constants::One : Multiplication::FromOperands(std::move(denominator));
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
  //  ASSERT(!unpacked_args.empty(), "Zero arguments left after canonicalization. Arguments
  //  were:\n{}",
  //         Join("\n", args, &FormatDebugTree));
  //
  //  if (unpacked_args.size() == 1) {
  //    // After canonicalization, only one term remained.
  //    // This could occur if the whole chain of multiplications evaluated to a constant.
  //    return unpacked_args.front();
  //  }
}

struct OrderVisitor {
  enum class RelativeOrder {
    LessThan,
    Equal,
    GreaterThan,
  };

  using ReturnType = RelativeOrder;

  using OrderOfConstants = TypeList<Float, Integer, Rational, Constant, Variable, Multiplication,
                                    Addition, Power, NaturalLog>;

  // Every type in the approved type list must appear here, or we get a compile error:
  static_assert(TypeListSize<OrderOfConstants>::Value == TypeListSize<ApprovedTypeList>::Value);

  template <typename A, typename B>
  RelativeOrder Apply(const A& a, const B& b) {
    if constexpr (!std::is_same_v<A, B>) {
      return IndexOfType<A, OrderOfConstants>::Value < IndexOfType<B, OrderOfConstants>::Value
                 ? RelativeOrder::LessThan
                 : RelativeOrder::GreaterThan;
    } else {
      // Otherwise call a method that compares equivalent types:
      // This will generate a compiler error if we forget types here.
      return Compare(a, b);
    }
  }

  // This visitor applies to any two members of `OrderOfConstants` that are _not_ the same type.
  template <typename Numeric>
  std::enable_if_t<ContainsTypeHelper<Numeric, Float, Integer, Rational, Constant>, RelativeOrder>
  Compare(const Numeric& a, const Numeric& b) const {
    if (a < b) {
      return RelativeOrder::LessThan;
    } else if (b < a) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }

  template <typename Derived>
  RelativeOrder Compare(const NAryOp<Derived>& a, const NAryOp<Derived>& b) const {
    // For multiplication and addition, sort lexicographically by recursively invoking OrderVisitor.
    const std::vector<Expr>& args_a = a.Args();
    const std::vector<Expr>& args_b = b.Args();

    auto it_a = args_a.begin();
    auto it_b = args_b.end();
    for (; it_a != args_a.end() && it_b != args_b.end(); ++it_a, ++it_b) {
      const std::optional<RelativeOrder> result = VisitBinaryStruct(*it_a, *it_b, OrderVisitor{});
      ASSERT(result.has_value(), "Order visitor failed to implement a comparison");
      if (*result == RelativeOrder::LessThan) {
        return RelativeOrder::LessThan;
      } else if (*result == RelativeOrder::GreaterThan) {
        return RelativeOrder::GreaterThan;
      }
    }
    if (it_a == args_a.end() && it_b != args_b.end()) {
      return RelativeOrder::LessThan;  // `a` is shorter:
    } else if (it_a != args_a.end() && it_b == args_b.end()) {
      return RelativeOrder::GreaterThan;  //  `b` is shorter
    }
    return RelativeOrder::Equal;  //  they are equal
  }

  RelativeOrder Compare(const Power& a, const Power& b) const {
    const std::optional<RelativeOrder> base_order =
        VisitBinaryStruct(a.Base(), b.Base(), OrderVisitor{});
    ASSERT(base_order.has_value());
    if (*base_order == RelativeOrder::LessThan) {
      return RelativeOrder::LessThan;
    } else if (*base_order == RelativeOrder::GreaterThan) {
      return RelativeOrder::GreaterThan;
    }
    // Otherwise order is determined by the exponent:
    const std::optional<RelativeOrder> exponent_order =
        VisitBinaryStruct(a.Exponent(), b.Exponent(), OrderVisitor{});
    ASSERT(exponent_order.has_value());
    return *exponent_order;
  }

  RelativeOrder Compare(const NaturalLog& a, const NaturalLog& b) const {
    const std::optional<RelativeOrder> arg_order =
        VisitBinaryStruct(a.Inner(), b.Inner(), OrderVisitor{});
    ASSERT(arg_order.has_value());
    return *arg_order;
  }

  RelativeOrder Compare(const Variable& a, const Variable& b) const {
    if (a.GetName() < b.GetName()) {
      return RelativeOrder::LessThan;
    } else if (a.GetName() > b.GetName()) {
      return RelativeOrder::GreaterThan;
    }
    return RelativeOrder::Equal;
  }
};

struct ExprEquality {
  bool operator()(const Expr& a, const Expr& b) const { return a.IsIdenticalTo(b); }
};

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
  } else if (std::optional<Integer> as_int = rational_term.TryConvertToInteger(); as_int) {
    args.push_back(Integer::Create(*as_int));
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

  if (args.empty()) {
    return Constants::One;
  } else if (args.size() == 1) {
    return args.front();
  }
  return MakeExpr<Multiplication>(std::move(args));
}

}  // namespace math
