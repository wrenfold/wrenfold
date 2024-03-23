// Copyright 2023 Gareth Cross
#include "operations.h"

#include <unordered_map>

#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"
#include "wf/matrix_expression.h"
#include "wf/substitute.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4702)  //  incorrectly labeled unreachable code
#endif

namespace wf {

// Visitor that performs a substitution.
// TargetExpressionType is the concrete type of the expression we are replacing.
template <typename Derived, typename TargetExpressionType>
struct substitute_visitor_base {
  explicit substitute_visitor_base(const TargetExpressionType& target,
                                   const scalar_expr& replacement)
      : target(target), replacement(replacement) {}

  scalar_expr operator()(const scalar_expr& expr) { return visit(expr, *this); }
  boolean_expr operator()(const boolean_expr& expr) { return visit(expr, *this); }

  matrix_expr operator()(const matrix_expr& expr) { return map_matrix_expression(expr, *this); }

  compound_expr operator()(const compound_expr& expr) {
    return map_compound_expressions(expr, *this);
  }

  // The argument is neither an addition nor a multiplication:
  // `X` can be scalar_expr or boolean_expr.
  template <typename Arg, typename X>
  X operator()(const Arg& other, const X& input_expression) {
    if constexpr (std::is_same_v<TargetExpressionType, Arg>) {
      if (are_identical(target, other)) {
        // Exact match, so replace it:
        return replacement;
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
  const TargetExpressionType& target;
  const scalar_expr& replacement;
};

template <typename Target>
struct substitute_visitor : substitute_visitor_base<substitute_visitor<Target>, Target> {
  // Standard substitute visitor does not allow partial matching.
  constexpr static bool performs_partial_substitution = false;

  // Inherit constructor.
  using substitute_visitor_base<substitute_visitor<Target>, Target>::substitute_visitor_base;
};

// Specialization to allow partial substitution in additions.
struct substitute_add_visitor : substitute_visitor_base<substitute_add_visitor, addition> {
  constexpr static bool performs_partial_substitution = true;

  substitute_add_visitor(const addition& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement), target_parts(target) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression,
                              const addition& candidate) const {
    // Create map representation for the input:
    addition_parts input_parts{candidate};

    if (target_parts.float_term.has_value()) {
      if (!input_parts.float_term ||
          !are_identical(*input_parts.float_term, *target_parts.float_term)) {
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
      if (it == input_parts.terms.end() || !it->second.is_identical_to(target_coeff)) {
        // One of the terms in the target is missing in the input, so we can't substitute:
        return input_expression;
      }
      input_parts.terms.erase(it);  //  Remove it from the input.
    }

    // Remove the rational part:
    input_parts.rational_term = input_parts.rational_term - target_parts.rational_term;

    // Add in the replacement and build a new addition:
    input_parts.add_terms(replacement);
    return input_parts.create_addition();
  }

 private:
  const addition_parts target_parts;
};

// Specialization for doing partial substitution in multiplications.
// This allows replacing parts of a product, for example:
//  Replace `x * y` in `x**3 * y**2 * 5` with `z` to obtain `x * z**2 * 5`.
struct substitute_mul_visitor : substitute_visitor_base<substitute_mul_visitor, multiplication> {
  constexpr static bool performs_partial_substitution = true;

  substitute_mul_visitor(const multiplication& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement), target_parts(target, true) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression,
                              const multiplication& candidate) const {
    // Take this multiplication and break it into constituent parts.
    // TODO: Should we just store multiplications pre-factored in this format?
    multiplication_parts input_parts{candidate, true};

    if (std::holds_alternative<float_constant>(input_parts.numeric_coeff)) {
      if (input_parts.numeric_coeff != target_parts.numeric_coeff) {
        // Don't allow substitutions that perform float operations.
        // (Unless the float coefficients match exactly, then we allow it.)
        return input_expression;
      } else {
        // The float components match, so divide it out and proceed w/ the rest of the replacement:
        input_parts.numeric_coeff = rational_constant{1, 1};
      }
    }

    // Determine the maximum number of times we can divide the exponent of the target
    // into the input multiplication.
    std::optional<integer_constant> max_valid_integer_exponent{};
    for (const auto& [base, exponent] : target_parts.terms) {
      auto it = input_parts.terms.find(base);
      if (it == input_parts.terms.end()) {
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
    for (const auto& [base, exponent] : target_parts.terms) {
      auto it = input_parts.terms.find(base);
      WF_ASSERT(it != input_parts.terms.end());
      it->second = it->second - (exponent * max_valid_exponent.value());
    }

    // Insert the replacement
    const scalar_expr replacement_exp(max_valid_exponent);
    if (const auto [it, was_inserted] = input_parts.terms.emplace(replacement, replacement_exp);
        !was_inserted) {
      it->second = it->second + replacement_exp;
    }

    input_parts.normalize_coefficients();
    return input_parts.create_multiplication();
  }

 protected:
  const multiplication_parts target_parts;
};

// Specialization for power so we can match.
// There is a lot of overlap w/ the substitute_mul_visitor - since any power is just a
// multiplication w/ one term. These can probably be unified somehow.
struct substitute_pow_visitor : substitute_visitor_base<substitute_pow_visitor, power> {
  constexpr static bool performs_partial_substitution = true;

  substitute_pow_visitor(const power& target, const scalar_expr& replacement)
      : substitute_visitor_base(target, replacement) {}

  scalar_expr attempt_partial(const scalar_expr& input_expression, const power& candidate) const {
    const scalar_expr& target_base = target.base();
    const scalar_expr& target_exponent = target.exponent();
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
          return power::create(replacement, ratio) * power::create(candidate_base, new_exponent);
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
          return power::create(replacement, int_part.value()) *
                 power::create(candidate_base, new_exponent);
        }
      }
    } else {
      // See if the exponent is an integer multiple of the target exponent.
      // TODO: De-duplicate this block with the equivalent section in the addition above.
      const scalar_expr multiple = candidate.exponent() / target_exponent;
      if (const integer_constant* const as_int = get_if<const integer_constant>(multiple);
          as_int != nullptr) {
        return power::create(replacement, multiple);
      } else if (const rational_constant* const as_rational =
                     get_if<const rational_constant>(multiple);
                 as_rational != nullptr) {
        const auto [int_part, frac_remainder] = as_rational->normalized();
        if (int_part.is_zero()) {
          // Can't do a full division
          return input_expression;
        }
        return power::create(replacement, int_part.value()) *
               power::create(candidate_base, target_exponent * scalar_expr(frac_remainder));
      }
    }

    return input_expression;
  }
};

template <typename T>
struct sub_visitor_type {
  using type = substitute_visitor<T>;
};
template <>
struct sub_visitor_type<addition> {
  using type = substitute_add_visitor;
};
template <>
struct sub_visitor_type<multiplication> {
  using type = substitute_mul_visitor;
};
template <>
struct sub_visitor_type<power> {
  using type = substitute_pow_visitor;
};

template <typename X>
X substitute_impl(const X& input, const scalar_expr& target, const scalar_expr& replacement) {
  return visit(target, [&](const auto& target_concrete) -> X {
    using T = std::decay_t<decltype(target_concrete)>;
    // Don't allow the target type to be a numeric literal:
    using disallowed_types =
        type_list<integer_constant, float_constant, rational_constant, boolean_constant>;
    if constexpr (type_list_contains_v<T, disallowed_types>) {
      throw type_error("Cannot perform a substitution with target type: {}", T::name_str);
    } else {
      using visitor_type = typename sub_visitor_type<T>::type;
      return visit(input, visitor_type{target_concrete, replacement});
    }
  });
}

scalar_expr substitute(const scalar_expr& input, const scalar_expr& target,
                       const scalar_expr& replacement) {
  return substitute_impl(input, target, replacement);
}

boolean_expr substitute(const boolean_expr& input, const scalar_expr& target,
                        const scalar_expr& replacement) {
  return substitute_impl(input, target, replacement);
}

static substitute_variables_visitor create_subs_visitor(
    const absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs) {
  substitute_variables_visitor visitor{};
  for (const auto& [target, replacement] : pairs) {
    if (!target.is_type<variable, compound_expression_element>()) {
      throw type_error("Input needs to be type `{}` or `{}`, received type `{}`: {}",
                       variable::name_str, compound_expression_element::name_str,
                       target.type_name(), target);
    }
    visitor.add_substitution(target, replacement);
  }
  return visitor;
}

scalar_expr substitute_variables(const scalar_expr& input,
                                 absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs) {
  return create_subs_visitor(pairs)(input);
}

matrix_expr substitute_variables(
    const matrix_expr& input, const absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs) {
  return map_matrix_expression(input, create_subs_visitor(pairs));
}

void substitute_variables_visitor::add_substitution(const scalar_expr& target,
                                                    scalar_expr replacement) {
  if (target.is_type<variable>()) {
    add_substitution(get_unchecked<const variable>(target), std::move(replacement));
  } else if (target.is_type<compound_expression_element>()) {
    add_substitution(get_unchecked<const compound_expression_element>(target),
                     std::move(replacement));
  } else {
    throw type_error(
        "Only expressions of type `{}` and `{}` may be used with substitute_variables_visitor.",
        variable::name_str, compound_expression_element::name_str);
  }
}

void substitute_variables_visitor::add_substitution(variable variable, scalar_expr replacement) {
  cache_.clear();  //  No longer valid when new expressions are added.
  const auto [it, was_inserted] =
      variable_substitutions_.emplace(std::move(variable), std::move(replacement));
  WF_ASSERT(was_inserted, "Variable already exists in the substitution list: {}",
            it->first.to_string());
}

void substitute_variables_visitor::add_substitution(compound_expression_element element,
                                                    scalar_expr replacement) {
  cache_.clear();  //  No longer valid when new expressions are added.
  const auto [it, was_inserted] =
      element_substitutions_.emplace(std::move(element), std::move(replacement));
  WF_ASSERT(was_inserted, "Element already exists in the substitution list: {}", it->first.index());
}

scalar_expr substitute_variables_visitor::operator()(const scalar_expr& expression) {
  if (const auto it = cache_.find(expression); it != cache_.end()) {
    return it->second;
  }
  scalar_expr result = visit(expression, *this);
  const auto [it_inserted, _] = cache_.emplace(expression, std::move(result));
  return it_inserted->second;
}

matrix_expr substitute_variables_visitor::operator()(const matrix_expr& expression) {
  return map_matrix_expression(expression, *this);
}

compound_expr substitute_variables_visitor::operator()(const compound_expr& expression) {
  return map_compound_expressions(expression, *this);
}

boolean_expr substitute_variables_visitor::operator()(const boolean_expr& expression) {
  return visit(expression, *this);
}

template <typename T, typename X>
X substitute_variables_visitor::operator()(const T& concrete, const X& abstract) {
  if constexpr (std::is_same_v<T, variable>) {
    if (const auto it = variable_substitutions_.find(concrete);
        it != variable_substitutions_.end()) {
      return it->second;
    }
  } else if constexpr (std::is_same_v<T, compound_expression_element>) {
    if (const auto it = element_substitutions_.find(concrete); it != element_substitutions_.end()) {
      return it->second;
    } else {
      return concrete.map_children(*this);
    }
  } else if constexpr (!T::is_leaf_node) {
    return concrete.map_children(*this);
  }
  return abstract;
}

}  // namespace wf

#ifdef _MSC_VER
#pragma warning(pop)
#endif
