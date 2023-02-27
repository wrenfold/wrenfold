// Copyright 2023 Gareth Cross
#include "substitute.h"

#include <unordered_map>
#include <unordered_set>

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "hashing.h"

namespace math {

// Visitor that performs a substitution.
// TargetExpressionType is the concrete type of the expression we are replacing.
template <typename TargetExpressionType>
struct SubstituteVisitor {
 public:
  explicit SubstituteVisitor(const Expr& input_expression, const TargetExpressionType& target,
                             const Expr& replacement)
      : input_expression(input_expression), target(target), replacement(replacement) {}

  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;
  using ReturnType = Expr;

  Expr Apply(const Addition& add) {
    if constexpr (std::is_same_v<TargetExpressionType, Addition>) {
      const Addition& target_addition = target;
      if (target_addition.IsIdenticalToImplTyped(add)) {
        // exact match
        return replacement;
      }

      // We should check if this addition is a superset of the target expression:
      std::unordered_set<Expr, HashObject, ExprEquality> remainder{add.begin(), add.end()};
      const bool contains_target = std::all_of(target_addition.begin(), target_addition.end(),
                                               [&remainder](const Expr& expr) {
                                                 const auto it = remainder.find(expr);
                                                 if (it != remainder.end()) {
                                                   remainder.erase(it);
                                                   return true;
                                                 } else {
                                                   return false;
                                                 }
                                               });

      // Traverse the target and check if all of its terms appear in `add`.
      if (contains_target) {
        std::vector<Expr> add_arguments;
        add_arguments.reserve(remainder.size() + 1);
        add_arguments.insert(add_arguments.end(), remainder.begin(), remainder.end());
        for (Expr& expr : add_arguments) {
          expr = VisitStruct(expr, SubstituteVisitor{expr, target, replacement});
        }
        add_arguments.push_back(replacement);
        return Addition::FromOperands(add_arguments);
      }
    }

    // If the target is not an addition, we can just map here.
    // The result will be a new addition w/ whatever changes were made.
    return MapChildren(add, [&](const Expr& child) {
      return VisitStruct(child, SubstituteVisitor{child, target, replacement});
    });
  }

  Expr Apply(const Multiplication& mul) {
    if constexpr (std::is_same_v<TargetExpressionType, Multiplication>) {
      const Multiplication& target_mul = target;
      if (target_mul.IsIdenticalToImplTyped(mul)) {
        // exact match
        return replacement;
      }

      // Take this multiplication and divide by the target multiplication:
      MultiplicationBuilder builder{mul.Arity() + target_mul.Arity()};
      for (const Expr& expr : mul) {
        builder.Multiply(expr);
      }
      for (const Expr& expr : target_mul) {
        // TODO: Do this w/o introducing the intermediate powers.
        builder.Multiply(Power::Create(expr, Constants::NegativeOne));
      }
      builder.Normalize();

      // If the division introduces a negative exponent, don't allow the replacement:
      const bool divided_cleanly =
          std::all_of(target_mul.begin(), target_mul.end(), [&builder](const Expr& expr) -> bool {
            const auto [base, _] = AsBaseAndExponent(expr);
            const auto it = builder.terms.find(base);
            return it == builder.terms.end() || !IsNegativeNumber(it->second);
          });

      // Traverse the target and check if all of its terms appear in `add`.
      if (divided_cleanly) {
        builder.Multiply(replacement);
        builder.Normalize();
        return builder.CreateMultiplication(std::vector<Expr>{});
      }
    }

    return MapChildren(mul, [&](const Expr& child) {
      return VisitStruct(child, SubstituteVisitor{child, target, replacement});
    });
  }

  // The argument is neither an addition nor a multiplication:
  template <typename Derived>
  std::enable_if_t<!std::is_same_v<Derived, Addition> && !std::is_same_v<Derived, Multiplication>,
                   Expr>
  Apply(const ExpressionImpl<Derived>& other) {
    if constexpr (std::is_same_v<TargetExpressionType, Derived>) {
      if (target.IsIdenticalToImplTyped(other.AsDerived())) {
        // Exact match, so replace it:
        return replacement;
      }
    }
    // If these expressions don't match, and the target has no sub-expressions, we can stop here.
    if constexpr (ExpressionImpl<Derived>::IsLeaf()) {
      return input_expression;
    } else {
      // Otherwise we substitute in every child:
      return MapChildren(other, [&](const Expr& child) {
        return VisitStruct(child, SubstituteVisitor{child, target, replacement});
      });
    }
  }

 protected:
  const Expr& input_expression;
  const TargetExpressionType& target;
  const Expr& replacement;
};

Expr Substitute(const Expr& input, const Expr& target, const Expr& replacement) {
  // Visit `target` to determine the underlying type, then visit the input w/ SubstituteVisitor:
  return detail::VisitLambdaWithPolicy<VisitorPolicy::CompileError>(
      target, [&](const auto& target) -> Expr {
        using T = std::decay_t<decltype(target)>;
        // Don't allow the target type to be a numeric literal:
        if constexpr (std::is_same_v<T, Integer> || std::is_same_v<T, Float> ||
                      std::is_same_v<T, Rational>) {
          throw TypeError("Cannot perform a substitution with target type: {}", target.TypeName());
        } else {
          return VisitStruct(input, SubstituteVisitor<T>{input, target, replacement});
        }
      });
}

}  // namespace math
