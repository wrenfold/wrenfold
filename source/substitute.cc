// Copyright 2023 Gareth Cross
#include "operations.h"

#include <unordered_map>

#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "hashing.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4702)  //  incorrectly labeled unreachable code
#endif

namespace math {

// Visitor that performs a substitution.
// TargetExpressionType is the concrete type of the expression we are replacing.
template <typename Derived, typename TargetExpressionType>
struct SubstituteVisitorBase {
 public:
  explicit SubstituteVisitorBase(const TargetExpressionType& target, const Expr& replacement)
      : target(target), replacement(replacement) {}

  using Policy = VisitorPolicy::CompileError;
  using ReturnType = Expr;

  // The argument is neither an addition nor a multiplication:
  template <typename Arg>
  Expr Apply(const Expr& input_expression, const Arg& other) {
    if constexpr (std::is_same_v<TargetExpressionType, Arg>) {
      if (target.IsIdenticalToImplTyped(other.AsDerived())) {
        // Exact match, so replace it:
        return replacement;
      }
      if constexpr (Derived::PerformsPartialSubstitution) {
        // The derived type supports looking for partial matches, so try that:
        Expr partial_sub = static_cast<Derived&>(*this).AttemptPartial(input_expression, other);
        // Irrespective of whether that succeeded, one of the children may still contain the
        // expression we are searching for as well:
        return VisitChildren(partial_sub, static_cast<Derived&>(*this));
      }
    }
    // If these expressions don't match, and the target has no sub-expressions, we can stop here.
    if constexpr (Arg::IsLeafStatic()) {
      return input_expression;
    } else {
      // Otherwise we substitute in every child:
      return MapChildren(other, [this](const Expr& child) {
        return VisitStruct(child, static_cast<Derived&>(*this));
      });
    }
  }

 protected:
  const TargetExpressionType& target;
  const Expr& replacement;
};

template <typename Target>
struct SubstituteVisitor : public SubstituteVisitorBase<SubstituteVisitor<Target>, Target> {
  // Standard substitute visitor does not allow partial matching.
  constexpr static bool PerformsPartialSubstitution = false;

  // Inherit constructor.
  using SubstituteVisitorBase<SubstituteVisitor<Target>, Target>::SubstituteVisitorBase;
};

// Specialization to allow partial substitution in additions.
struct SubstituteAddVisitor : public SubstituteVisitorBase<SubstituteAddVisitor, Addition> {
 public:
  constexpr static bool PerformsPartialSubstitution = true;

  SubstituteAddVisitor(const Addition& target, const Expr& replacement)
      : SubstituteVisitorBase(target, replacement), target_parts(target) {}

  Expr AttemptPartial(const Expr& input_expression, const Addition& candidate) {
    // Create map representation for the input:
    AdditionParts input_parts{candidate};

    if (target_parts.float_term.has_value()) {
      if (!input_parts.float_term ||
          !input_parts.float_term->IsIdenticalToImplTyped(*target_parts.float_term)) {
        // Don't allow substitutions that perform float operations.
        return input_expression;
      } else {
        // The float components match, so subtract it out and proceed w/ the rest of the
        // replacement:
        input_parts.float_term.reset();
      }
    }

    // TODO: Support changing the sign here. So `x + y` and `-x - y` should both match `x + y`.
    for (const auto& [target_mul, target_coeff] : target_parts.terms) {
      auto it = input_parts.terms.find(target_mul);
      if (it == input_parts.terms.end() || !it->second.IsIdenticalTo(target_coeff)) {
        // One of the terms in the target is missing in the input, so we can't substitute:
        return input_expression;
      }
      input_parts.terms.erase(it);  //  Remove it from the input.
    }

    // Remove the rational part:
    input_parts.rational_term = input_parts.rational_term - target_parts.rational_term;

    // Add in the replacement and build a new addition:
    input_parts.Add(replacement);
    return input_parts.CreateAddition({});
  }

 private:
  const AdditionParts target_parts;
};

// Specialization for doing partial substitution in multiplications.
// This allows replacing parts of a product, for example:
//  Replace `x * y` in `x**3 * y**2 * 5` with `z` to obtain `x * z**2 * 5`.
struct SubstituteMulVisitor : public SubstituteVisitorBase<SubstituteMulVisitor, Multiplication> {
 public:
  constexpr static bool PerformsPartialSubstitution = true;

  SubstituteMulVisitor(const Multiplication& target, const Expr& replacement)
      : SubstituteVisitorBase(target, replacement), target_parts(target, true) {}

  Expr AttemptPartial(const Expr& input_expression, const Multiplication& candidate) {
    // Take this multiplication and break it into constituent parts.
    // TODO: Should we just store multiplications pre-factored in this format?
    MultiplicationParts input_parts{candidate, true};

    if (target_parts.float_coeff.has_value()) {
      if (!input_parts.float_coeff ||
          !input_parts.float_coeff->IsIdenticalToImplTyped(*target_parts.float_coeff)) {
        // Don't allow substitutions that perform float operations.
        // (Unless the float coefficients match exactly, then we allow it.)
        return input_expression;
      } else {
        // The float components match, so divide it out and proceed w/ the rest of the replacement:
        input_parts.float_coeff.reset();
      }
    }

    // Determine the maximum number of times we can divide the exponent of the target
    // into the input multiplication.
    std::optional<Integer> max_valid_integer_exponent{};
    for (const auto& [base, exponent] : target_parts.terms) {
      auto it = input_parts.terms.find(base);
      if (it == input_parts.terms.end()) {
        return input_expression;
      }

      // See how many times we can divide term into the target expression
      const Expr multiple = it->second / exponent;
      if (const Integer* const as_int = TryCast<Integer>(multiple); as_int != nullptr) {
        // We do `abs` here so that doing a substitution like:
        // 1 / (x*x*y*y*y) replacing [x*y -> w] produces 1 / (w*w*y)
        // Instead of producing (if we used the sign): x / w^3
        // In other words, we don't want to switch the signs of exponents - so take the integer
        // power closest to zero as `max_valid_integer_exponent`.
        if (!max_valid_integer_exponent.has_value() ||
            as_int->Abs() < max_valid_integer_exponent.value().Abs()) {
          max_valid_integer_exponent = *as_int;
        }
      } else if (const Rational* const as_rational = TryCast<Rational>(multiple);
                 as_rational != nullptr) {
        const auto [int_part, _] = as_rational->Normalize();
        // Same rationale for `abs` as above for integers:
        if (!max_valid_integer_exponent.has_value() ||
            int_part.Abs() < max_valid_integer_exponent.value().Abs()) {
          max_valid_integer_exponent = int_part;
        }
      } else {
        // If we can't get an integer multiple, this division won't work.
        return input_expression;
      }
    }

    // Deduct the replaced expression from the input:
    const Integer max_valid_exponent = max_valid_integer_exponent.value();
    for (const auto& [base, exponent] : target_parts.terms) {
      auto it = input_parts.terms.find(base);
      ASSERT(it != input_parts.terms.end());
      it->second = it->second - (exponent * max_valid_exponent.GetValue());
    }

    // Insert the replacement
    const auto replacement_exp = Integer::Create(max_valid_exponent);
    const auto [it, was_inserted] = input_parts.terms.emplace(replacement, replacement_exp);
    if (!was_inserted) {
      it->second = it->second + replacement_exp;
    }

    input_parts.Normalize();
    return input_parts.CreateMultiplication(std::vector<Expr>{});
  }

 protected:
  const MultiplicationParts target_parts;
};

// Specialization for power so we can match.
// There is a lot of overlap w/ the SubstituteMulVisitor - since any power is just a multiplication
// w/ one term. These can probably be unified somehow.
struct SubstitutePowVisitor : public SubstituteVisitorBase<SubstitutePowVisitor, Power> {
 public:
  constexpr static bool PerformsPartialSubstitution = true;

  SubstitutePowVisitor(const Power& target, const Expr& replacement)
      : SubstituteVisitorBase(target, replacement) {}

  Expr AttemptPartial(const Expr& input_expression, const Power& candidate) {
    const Expr& target_base = target.Base();
    const Expr& target_exponent = target.Exponent();
    const Expr& candidate_base = candidate.Base();

    // If the bases don't match, there can't be a valid substitution:
    if (!target_base.IsIdenticalTo(candidate_base)) {
      return input_expression;
    }

    // If the exponent is an addition, it might contain a multiple of our target exponent.
    if (const Addition* const add_exp = TryCast<Addition>(candidate.Exponent());
        add_exp != nullptr) {
      const auto [target_exp_coeff, target_exp_mul] = AsCoefficientAndMultiplicand(target_exponent);

      // Break into parts:
      AdditionParts parts{*add_exp};
      auto it = parts.terms.find(target_exp_mul);
      if (it != parts.terms.end()) {
        // Our exponent appears in the addition. See if it divides cleanly:
        const Expr ratio = it->second / target_exp_coeff;
        if (const Integer* const as_int = TryCast<Integer>(ratio); as_int != nullptr) {
          // It divides evenly. This case handles things like:
          // x**(3*y + 5) replacing [x**y -> w] producing w**3 * x**5
          parts.terms.erase(it);
          // Put the exponent back together and swap in the replacement:
          Expr new_exponent = parts.CreateAddition({});
          return Power::Create(replacement, ratio) * Power::Create(candidate_base, new_exponent);
        } else if (const Rational* const as_rational = TryCast<Rational>(ratio);
                   as_rational != nullptr) {
          const auto [int_part, _] = as_rational->Normalize();
          if (!int_part.GetValue()) {
            // If int_part is zero, the rest of this logic just wastes time.
            return input_expression;
          }
          // Subtract the integer part from the exponent.
          // For example, this would handle the replacement:
          // x**(4/3*y + z) replacing [x**y -> w] producing w * x**(1/3y + z)
          it->second = it->second - target_exp_coeff * int_part.GetValue();
          Expr new_exponent = parts.CreateAddition({});
          return Power::Create(replacement, int_part.GetValue()) *
                 Power::Create(candidate_base, new_exponent);
        }
      }
    } else {
      // See if the exponent is an integer multiple of the target exponent.
      // TODO: De-duplicate this block with the equivalent section in the addition above.
      const Expr multiple = candidate.Exponent() / target_exponent;
      if (const Integer* const as_int = TryCast<Integer>(multiple); as_int != nullptr) {
        return Power::Create(replacement, multiple);
      } else if (const Rational* const as_rational = TryCast<Rational>(multiple);
                 as_rational != nullptr) {
        const auto [int_part, frac_remainder] = as_rational->Normalize();
        if (!int_part.GetValue()) {
          // Can't do a full division
          return input_expression;
        }
        return Power::Create(replacement, int_part.GetValue()) *
               Power::Create(candidate_base, target_exponent * Rational::Create(frac_remainder));
      }
    }

    return input_expression;
  }
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
        } else if constexpr (std::is_same_v<T, Addition>) {
          return VisitStruct(input, SubstituteAddVisitor{target, replacement});
        } else if constexpr (std::is_same_v<T, Multiplication>) {
          return VisitStruct(input, SubstituteMulVisitor{target, replacement});
        } else if constexpr (std::is_same_v<T, Power>) {
          return VisitStruct(input, SubstitutePowVisitor{target, replacement});
        } else {
          return VisitStruct(input, SubstituteVisitor<T>{target, replacement});
        }
      });
}

}  // namespace math

#ifdef _MSC_VER
#pragma warning(pop)
#endif
