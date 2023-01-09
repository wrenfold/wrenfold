// Copyright 2022 Gareth Cross
#include "expressions/addition.h"

#include <unordered_map>

#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "hashing.h"
#include "ordering.h"

namespace math {

// Visitor that accumulates integers/rationals.
struct RationalAccumulator {
  using ReturnType = void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  void Apply(const Rational& r) { result = result + r; }
  void Apply(const Integer& i) { result = result + static_cast<Rational>(i); }

  Rational result{0, 1};
};

// Add integer + rational constants.
// Removes them from the vector during processing.
static Rational AddIntegersAndRationals(std::vector<Expr>& input) {
  RationalAccumulator accumulator{};
  const auto new_end = std::remove_if(input.begin(), input.end(), [&accumulator](const Expr& expr) {
    return VisitStruct(expr, accumulator).has_value();
  });
  input.erase(new_end, input.end());
  return accumulator.result;
}

static std::optional<Float> AddFloats(std::vector<Expr>& input) {
  std::optional<Float> sum{};
  const auto new_end = std::remove_if(input.begin(), input.end(), [&sum](const Expr& expr) {
    return VisitLambda(expr,
                       [&](const Float& f) {
                         if (!sum) {
                           sum = f;
                         } else {
                           sum = *sum + f;
                         }
                       })
        .has_value();
  });
  input.erase(new_end, input.end());
  return sum;
}

Addition::Addition(std::vector<Expr> args) : NAryOp(std::move(args)) {}

Expr Addition::FromOperands(const std::vector<Expr>& args) {
  std::vector<Expr> unpacked_args{};
  unpacked_args.reserve(args.size());

  // Flatten arguments:
  for (const Expr& arg : args) {
    if (IsZero(arg)) {
      // Skip zeros.
      continue;
    }
    if (const Addition* const add = TryCast<Addition>(arg)) {
      const std::vector<Expr>& add_args = add->Args();
      unpacked_args.insert(unpacked_args.end(), add_args.begin(), add_args.end());
    } else {
      unpacked_args.push_back(arg);
    }
  }

  // Extract all constants:
  const Rational rational_term = AddIntegersAndRationals(unpacked_args);
  const std::optional<Float> float_term = AddFloats(unpacked_args);

  // Create a map from multiplicand -> coefficient.
  // TODO: Try the abseil map container.
  std::unordered_map<Expr, Expr, HashObject, ExprEquality> map{};
  map.reserve(args.size());

  // This will group terms w/ the same multiplicand by adding their coefficients:
  for (const Expr& expr : unpacked_args) {
    auto [coeff, multiplicand] = AsCoefficientAndMultiplicand(expr);
    const auto [it, was_inserted] = map.emplace(std::move(multiplicand), coeff);
    if (!was_inserted) {
      it->second = Addition::FromTwoOperands(it->second, coeff);
    }
  }

  // Remove anything where the coefficient worked out to zero:
  for (auto it = map.begin(); it != map.end();) {
    if (IsZero(it->second)) {
      it = map.erase(it);
    } else {
      ++it;
    }
  }

  // Create the result:
  unpacked_args.clear();
  if (float_term && float_term->GetValue() != 0.0) {
    unpacked_args.push_back(
        MakeExpr<Float>(float_term.value() + static_cast<Float>(rational_term)));
  } else if (rational_term.IsZero()) {
    // Skip it...
  } else {
    unpacked_args.push_back(Rational::Create(rational_term));
  }

  // Sort back into order:
  // TODO: Just store this representation:
  std::vector<std::pair<Expr, Expr>> coeff_mul;
  coeff_mul.reserve(args.size());
  std::copy(map.begin(), map.end(), std::back_inserter(coeff_mul));

  std::sort(coeff_mul.begin(), coeff_mul.end(), [](const auto& a, const auto& b) {
    const std::optional<OrderVisitor::RelativeOrder> order_mul =
        VisitBinaryStruct(a.first, b.first, OrderVisitor{});
    ASSERT(order_mul);
    if (*order_mul == OrderVisitor::RelativeOrder::LessThan) {
      return true;
    } else if (*order_mul == OrderVisitor::RelativeOrder::GreaterThan) {
      return false;
    }
    // Otherwise compare the coefficient as well:
    const std::optional<OrderVisitor::RelativeOrder> order_coeff =
        VisitBinaryStruct(a.second, b.second, OrderVisitor{});
    ASSERT(order_coeff);
    return *order_coeff == OrderVisitor::RelativeOrder::LessThan;
  });

  // Insert back into expected form (TODO: this is pretty inefficient...)
  for (auto [multiplicand, coeff] : coeff_mul) {
    unpacked_args.push_back(Multiplication::FromTwoOperands(coeff, multiplicand));
  }

  if (unpacked_args.empty()) {
    return Constants::Zero;
  } else if (unpacked_args.size() == 1) {
    return unpacked_args.front();
  }
  return MakeExpr<Addition>(std::move(unpacked_args));
}

}  // namespace math
