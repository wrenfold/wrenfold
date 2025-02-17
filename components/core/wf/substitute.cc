// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/substitute.h"

#include <unordered_map>

#include "wf/expression_cache.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/compound_expression_element.h"
#include "wf/expressions/variable.h"
#include "wf/matrix_expression.h"

#include "utility/scoped_trace.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4702)  //  incorrectly labeled unreachable code
#endif

namespace wf {

// Visitor that performs a substitution.
// TargetExpressionType is the concrete type of the expression we are replacing.
template <typename Derived, typename TargetConcreteExpressionType,
          typename ReplacementAbstractExpressionType>
struct substitute_visitor_base {
  explicit substitute_visitor_base(const TargetConcreteExpressionType& target,
                                   const ReplacementAbstractExpressionType& replacement)
      : target_(target), replacement_(replacement) {}

  template <typename X, typename = enable_if_inherits_expression_base_t<X>>
  X operator()(const X& expr) {
    return cache_.get_or_insert(expr, [this](const X& x) { return visit(x, *this); });
  }

  // The argument is neither an addition nor a multiplication:
  // `X` can be scalar_expr or boolean_expr.
  template <typename Arg, typename X>
  X operator()(const Arg& other, const X& input_expression) {
    if constexpr (std::is_same_v<TargetConcreteExpressionType, Arg>) {
      // Make sure `X` is scalar_expr or boolean_expr, depending on what we are replacing:
      if constexpr (std::is_same_v<ReplacementAbstractExpressionType, X>) {
        if (are_identical(target_, other)) {
          // Exact match, so replace it:
          return replacement_;
        }
      }

      if constexpr (Derived::performs_partial_substitution) {
        // The derived type supports looking for partial matches, so try that:
        scalar_expr partial_sub =
            static_cast<Derived&>(*this).attempt_partial(input_expression, other);

        // Irrespective of whether that succeeded, one of the children may still contain the
        // expression we are searching for as well:
        return visit(partial_sub, [this, &partial_sub](const auto& arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (T::is_leaf_node) {
            // This type has no children, so return the input expression unmodified:
            return partial_sub;
          } else {
            // This type does have children, so apply to all of them:
            return arg.map_children(static_cast<Derived&>(*this));
          }
        });
      }
    }
    // If these expressions don't match, and the target has no sub-expressions, we can stop here.
    if constexpr (Arg::is_leaf_node) {
      return input_expression;
    } else {
      // Otherwise we substitute in every child:
      return other.map_children(static_cast<Derived&>(*this));
    }
  }

 protected:
  const TargetConcreteExpressionType& target_;
  const ReplacementAbstractExpressionType& replacement_;

  wf::expression_cache cache_;
};

template <typename Target, typename Replacement>
struct substitute_visitor
    : substitute_visitor_base<substitute_visitor<Target, Replacement>, Target, Replacement> {
  // Standard substitute visitor does not allow partial matching.
  constexpr static bool performs_partial_substitution = false;

  // Inherit constructor.
  using substitute_visitor_base<substitute_visitor, Target, Replacement>::substitute_visitor_base;
};

// Specialization to allow partial substitution in additions.
struct substitute_add_visitor
    : substitute_visitor_base<substitute_add_visitor, addition, scalar_expr> {
  constexpr static bool performs_partial_substitution = true;

  substitute_add_visitor(const addition& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement), target_parts(target) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression,
                              const addition& candidate) const {
    // Create map representation for the input:
    addition_parts input_parts{candidate};

    if (std::holds_alternative<float_constant>(input_parts.coeff)) {
      if (input_parts.coeff != target_parts.coeff) {
        // Don't allow substitutions that perform float operations.
        // (Unless the float coefficients match exactly, then we allow it.)
        return input_expression;
      } else {
        // The float components match, so subtract it out and proceed w/ the rest of the
        // replacement:
        input_parts.coeff = integer_constant{0};
      }
    }

    // TODO: Support changing the sign here. So `x + y` and `-x - y` should both match `x + y`.
    for (const auto& [target_mul, target_coeff] : target_parts.terms) {
      auto it = input_parts.terms.find(target_mul);
      if (it == input_parts.terms.end() || !it->second.is_identical_to(target_coeff)) {
        // One of the terms in the target is missing in the input, so we can't substitute:
        return input_expression;
      }
      input_parts.terms.erase(it);  //  Remove it from the input.
    }

    // Remove the rational part:
    if (!std::holds_alternative<float_constant>(target_parts.coeff)) {
      input_parts.add_terms(-std::visit([](auto x) { return scalar_expr(x); }, target_parts.coeff));
    }

    // Add in the replacement and build a new addition:
    input_parts.add_terms(replacement_);
    return input_parts.create_addition();
  }

 private:
  const addition_parts target_parts;
};

// Specialization for doing partial substitution in multiplications.
// This allows replacing parts of a product, for example:
//  Replace `x * y` in `x**3 * y**2 * 5` with `z` to obtain `x * z**2 * 5`.
struct substitute_mul_visitor
    : substitute_visitor_base<substitute_mul_visitor, multiplication, scalar_expr> {
  constexpr static bool performs_partial_substitution = true;

  substitute_mul_visitor(const multiplication& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement), target_parts(target, true) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression,
                              const multiplication& candidate) const {
    // Take this multiplication and break it into constituent parts.
    // TODO: Should we just store multiplications pre-factored in this format?
    multiplication_parts input_parts{candidate, true};

    if (std::holds_alternative<float_constant>(input_parts.coeff())) {
      if (input_parts.coeff() != target_parts.coeff()) {
        // Don't allow substitutions that perform float operations.
        // (Unless the float coefficients match exactly, then we allow it.)
        return input_expression;
      } else {
        // The float components match, so divide it out and proceed w/ the rest of the replacement:
        input_parts.coeff() = integer_constant{1};
      }
    }

    // Determine the maximum number of times we can divide the exponent of the target
    // into the input multiplication.
    std::optional<integer_constant> max_valid_integer_exponent{};
    for (const auto& [base, exponent] : target_parts.terms()) {
      const auto it = input_parts.terms().find(base);
      if (it == input_parts.terms().end()) {
        return input_expression;
      }

      // See how many times we can divide term into the target expression
      const scalar_expr multiple = it->second / exponent;
      if (const integer_constant* const as_int = get_if<const integer_constant>(multiple);
          as_int != nullptr) {
        // We do `abs` here so that doing a substitution like:
        // 1 / (x*x*y*y*y) replacing [x*y -> w] produces 1 / (w*w*y)
        // Instead of producing (if we used the sign): x / w^3
        // In other words, we don't want to switch the signs of exponents - so take the integer
        // power closest to zero as `max_valid_integer_exponent`.
        if (!max_valid_integer_exponent.has_value() ||
            as_int->abs() < max_valid_integer_exponent.value().abs()) {
          max_valid_integer_exponent = *as_int;
        }
      } else if (const rational_constant* const as_rational =
                     get_if<const rational_constant>(multiple);
                 as_rational != nullptr) {
        const auto [int_part, _] = as_rational->normalized();
        // Same rationale for `abs` as above for integers:
        if (!max_valid_integer_exponent.has_value() ||
            int_part.abs() < max_valid_integer_exponent.value().abs()) {
          max_valid_integer_exponent = int_part;
        }
      } else {
        // If we can't get an integer multiple, this division won't work.
        return input_expression;
      }
    }

    // Deduct the replaced expression from the input:
    const integer_constant max_valid_exponent = max_valid_integer_exponent.value();
    for (const auto& [base, exponent] : target_parts.terms()) {
      auto it = input_parts.terms().find(base);
      WF_ASSERT(it != input_parts.terms().end());
      it->second = it->second - (exponent * max_valid_exponent.value());
    }

    // Insert the replacement
    const scalar_expr replacement_exp(max_valid_exponent);
    if (const auto [it, was_inserted] = input_parts.terms().emplace(replacement_, replacement_exp);
        !was_inserted) {
      it->second = it->second + replacement_exp;
    }

    input_parts.normalize_coefficients();
    return std::move(input_parts).create_multiplication();
  }

 protected:
  const multiplication_parts target_parts;
};

// Specialization for power so we can match.
// There is a lot of overlap w/ the substitute_mul_visitor - since any power is just a
// multiplication w/ one term. These can probably be unified somehow.
struct substitute_pow_visitor
    : substitute_visitor_base<substitute_pow_visitor, power, scalar_expr> {
  constexpr static bool performs_partial_substitution = true;

  substitute_pow_visitor(const power& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression, const power& candidate) const {
    const scalar_expr& target_base = target_.base();
    const scalar_expr& target_exponent = target_.exponent();
    const scalar_expr& candidate_base = candidate.base();

    // If the bases don't match, there can't be a valid substitution:
    if (!target_base.is_identical_to(candidate_base)) {
      return input_expression;
    }

    // If the exponent is an addition, it might contain a multiple of our target exponent.
    if (const addition* const add_exp = get_if<const addition>(candidate.exponent());
        add_exp != nullptr) {
      const auto [target_exp_coeff, target_exp_mul] = as_coeff_and_mul(target_exponent);

      // Break into parts:
      addition_parts parts{*add_exp};
      if (auto it = parts.terms.find(target_exp_mul); it != parts.terms.end()) {
        // Our exponent appears in the addition. See if it divides cleanly:
        const scalar_expr ratio = it->second / target_exp_coeff;
        if (const integer_constant* const as_int = get_if<const integer_constant>(ratio);
            as_int != nullptr) {
          // It divides evenly. This case handles things like:
          // x**(3*y + 5) replacing [x**y -> w] producing w**3 * x**5
          parts.terms.erase(it);
          // Put the exponent back together and swap in the replacement:
          scalar_expr new_exponent = parts.create_addition();
          return power::create(replacement_, ratio) * power::create(candidate_base, new_exponent);
        } else if (const rational_constant* const as_rational =
                       get_if<const rational_constant>(ratio);
                   as_rational != nullptr) {
          const auto [int_part, _] = as_rational->normalized();
          if (int_part.is_zero()) {
            // If int_part is zero, the rest of this logic just wastes time.
            return input_expression;
          }
          // Subtract the integer part from the exponent.
          // For example, this would handle the replacement:
          // x**(4/3*y + z) replacing [x**y -> w] producing w * x**(1/3y + z)
          it->second = it->second - target_exp_coeff * int_part.value();
          scalar_expr new_exponent = parts.create_addition();
          return power::create(replacement_, int_part.value()) *
                 power::create(candidate_base, new_exponent);
        }
      }
    } else {
      // See if the exponent is an integer multiple of the target exponent.
      // TODO: De-duplicate this block with the equivalent section in the addition above.
      const scalar_expr multiple = candidate.exponent() / target_exponent;
      if (const integer_constant* const as_int = get_if<const integer_constant>(multiple);
          as_int != nullptr) {
        return power::create(replacement_, multiple);
      } else if (const rational_constant* const as_rational =
                     get_if<const rational_constant>(multiple);
                 as_rational != nullptr) {
        const auto [int_part, frac_remainder] = as_rational->normalized();
        if (int_part.is_zero()) {
          // Can't do a full division
          return input_expression;
        }
        return power::create(replacement_, int_part.value()) *
               power::create(candidate_base, target_exponent * scalar_expr(frac_remainder));
      }
    }

    return input_expression;
  }
};

template <typename T, typename X>
struct sub_visitor_type {
  using type = substitute_visitor<T, X>;
};
template <typename X>
struct sub_visitor_type<addition, X> {
  using type = substitute_add_visitor;
};
template <typename X>
struct sub_visitor_type<multiplication, X> {
  using type = substitute_mul_visitor;
};
template <typename X>
struct sub_visitor_type<power, X> {
  using type = substitute_pow_visitor;
};

bool substitute_variables_visitor::add_substitution(scalar_expr target, scalar_expr replacement) {
  if (target.is_type<variable, compound_expression_element, function_argument_variable,
                     unique_variable>()) {
    const auto [_, was_inserted] =
        substitutions_.emplace(std::move(target), std::move(replacement));
    return was_inserted;
  } else {
    throw type_error(
        "Only expressions of type `{}`, `{}`, `{}`, and `{}` may be used with "
        "substitute_variables_visitor.",
        variable::name_str, compound_expression_element::name_str,
        function_argument_variable::name_str, unique_variable::name_str);
  }
}

bool substitute_variables_visitor::contains_target_variable(const scalar_expr& target) const {
  return substitutions_.contains(target);
}

scalar_expr substitute_variables_visitor::operator()(const scalar_expr& expression) {
  if (const auto it = cache_.find(expression); it != cache_.end()) {
    return it->second;
  } else if (const auto replace_it = substitutions_.find(expression);
             replace_it != substitutions_.end()) {
    return replace_it->second;
  }
  scalar_expr result = visit(expression, *this);
  const auto [it_inserted, _] = cache_.emplace(expression, std::move(result));
  return it_inserted->second;
}

matrix_expr substitute_variables_visitor::operator()(const matrix_expr& expression) {
  return visit(expression, *this);
}

compound_expr substitute_variables_visitor::operator()(const compound_expr& expression) {
  return visit(expression, *this);
}

boolean_expr substitute_variables_visitor::operator()(const boolean_expr& expression) {
  return visit(expression, *this);
}

template <typename T, typename X>
X substitute_variables_visitor::operator()(const T& concrete, const X& abstract) {
  if constexpr (!T::is_leaf_node) {
    return concrete.map_children(*this);
  }
  return abstract;
}

// TODO: Move this somewhere common, maybe it should be a method on scalar_expr and boolean_expr?
struct is_leaf_visitor {
  template <typename T>
  constexpr bool operator()(const T&) const noexcept {
    return T::is_leaf_node;
  }
};

// To be eligible for the faster unordered substitution path:
//  - All targets must be variables (or compound expression elements).
//  - All replacements must be leaf nodes, and no replacement can be found in the target list.
template <typename X>
static bool is_valid_for_unordered_subs(const X& target, const X& replacement,
                                        const substitute_variables_visitor& visitor,
                                        const bool force_unordered_execution) {
  // This constexpr-if is temporary. It is here because we only support variables
  // of _scalar_ type for now, but that will change shortly.
  if constexpr (std::is_same_v<X, scalar_expr>) {
    if (!target.template is_type<variable, compound_expression_element>()) {
      return false;
    }

    if (visitor.contains_target_variable(target)) {
      // We can't force unordered execution if the target appears twice in the
      // operand list.
      return false;
    }
  }

  // Check if the replacement is a leaf node:
  if (const bool replacement_is_leaf = visit(replacement, is_leaf_visitor{});
      !replacement_is_leaf && !force_unordered_execution) {
    // Unless forced, we won't choose the unordered path here because there could be
    // a dependency between subsequent substitutions. We could check for such
    // dependencies, but we would need to scan the whole expression tree of
    // `replacement`.
    return false;
  }

  // If the replacement _is_ a leaf, we still need to check if it matches one
  // of our targets. If so, we have an order specific dependency.
  if constexpr (std::is_same_v<X, scalar_expr>) {
    return !visitor.contains_target_variable(replacement);
  } else {
    return true;
  }
}

static std::optional<substitute_variables_visitor> create_unordered_subs_visitor(
    const absl::Span<const scalar_or_boolean_pair> pairs, const bool force_unordered_execution) {
  substitute_variables_visitor visitor{};
  for (const auto& pair : pairs) {
    if (const scalar_pair* scalars = std::get_if<scalar_pair>(&pair); scalars != nullptr) {
      if (is_valid_for_unordered_subs(std::get<0>(*scalars), std::get<1>(*scalars), visitor,
                                      force_unordered_execution)) {
        visitor.add_substitution(std::get<0>(*scalars), std::get<1>(*scalars));
      } else {
        return std::nullopt;
      }
    } else {
      // We don't support boolean substitutions yet via this path:
      return std::nullopt;
    }
  }
  return {std::move(visitor)};
}

template <typename X1, typename X2>
X1 substitute_single_impl(const X1& input, const X2& target, const X2& replacement) {
  return visit(target, [&](const auto& target_concrete) -> X1 {
    using T = std::decay_t<decltype(target_concrete)>;
    // Don't allow the target type to be a numeric literal:
    using disallowed_types =
        type_list<integer_constant, float_constant, rational_constant, boolean_constant>;
    if constexpr (type_list_contains_v<T, disallowed_types>) {
      throw type_error("Cannot perform a substitution with target type: {}, target = {}",
                       T::name_str, target);
    } else {
      using visitor_type = typename sub_visitor_type<T, X2>::type;
      return visit(input, visitor_type{target_concrete, replacement});
    }
  });
}

any_expression substitute_any(any_expression input,
                              const absl::Span<const scalar_or_boolean_pair> pairs,
                              const bool force_unordered_execution) {
  WF_FUNCTION_TRACE();
  if (pairs.size() > 1) {
    // Unordered execution proffers no benefits if size is one.
    if (std::optional<substitute_variables_visitor> subs_variables_visitor =
            create_unordered_subs_visitor(pairs, force_unordered_execution);
        subs_variables_visitor.has_value()) {
      return std::visit(
          [&](const auto& x) -> any_expression { return subs_variables_visitor->operator()(x); },
          input);
    }
  }

  // Otherwise, take the slow (ordered) path:
  return std::visit(
      [&pairs](auto input_expr) -> any_expression {
        auto result = std::move(input_expr);
        for (const scalar_or_boolean_pair& pair : pairs) {
          // Visit the pair of either scalar or boolean expressions:
          result = std::visit(
              [&result](const auto& p) {
                return substitute_single_impl(result, std::get<0>(p), std::get<1>(p));
              },
              pair);
        }
        return result;
      },
      std::move(input));
}

}  // namespace wf

#ifdef _MSC_VER
#pragma warning(pop)
#endif
