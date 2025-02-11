// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <algorithm>

#include "wf/expression_visitor.h"

namespace wf {

// Visitor for collecting terms.
// Can transform x^2*y + x^2*pi + -x * 5 - cos(z) * x --> x^2 * (y + pi) + x * (-5 - cos(z))
struct collect_visitor {
  explicit collect_visitor(const absl::Span<const scalar_expr> terms) : collected_terms_(terms) {}

  scalar_expr operator()(const scalar_expr& x) { return visit(x, *this); }
  compound_expr operator()(const compound_expr& x) { return visit(x, *this); }
  boolean_expr operator()(const boolean_expr& x) { return visit(x, *this); }
  matrix_expr operator()(const matrix_expr& x) { return visit(x, *this); }

  scalar_expr collect_addition_terms(addition::container_type&& container) const {
    // iterate over the terms we want to search for:
    const scalar_expr& collected_term = collected_terms_.front();

    // Mapping from exponents of `collected_term` to the values multiplied by them.
    // For example, if given: x**2 * 3 + x*5 - x**2 * pi
    // This map would look like:
    //  2 --> [3]
    //  1 --> [5, pi]
    std::unordered_map<scalar_expr, addition::container_type, hash_struct<scalar_expr>,
                       is_identical_struct<scalar_expr>>
        exponents_to_mul{};

    const auto new_end =
        std::remove_if(container.begin(), container.end(), [&](const scalar_expr& child) {
          if (const multiplication* mul = get_if<const multiplication>(child); mul != nullptr) {
            // Look for relevant terms:
            std::optional<scalar_expr> exponent;
            const auto it =
                std::find_if(mul->begin(), mul->end(), [&](const scalar_expr& mul_term) {
                  if (auto [base, exp] = as_base_and_exp(mul_term);
                      base.is_identical_to(collected_term)) {
                    // found the base we want
                    exponent = std::move(exp);
                    return true;
                  }
                  return false;
                });

            if (it == mul->end()) {
              // The variable we are searching for doesn't appear in this term of the addition.
              return false;
            }

            // The remaining terms (after factoring out the collected term) all get put into
            // our table from exponents to coefficients of the collected term.
            // So for example, if we had found `x**5 * pi * y`, we'd insert `pi` and `y` into the
            // table.
            if (it != mul->end()) {
              multiplication::container_type mul_parts{};
              std::copy(mul->begin(), it, std::back_inserter(mul_parts));
              std::copy(std::next(it), mul->end(), std::back_inserter(mul_parts));
              auto reduced_mul = multiplication::from_operands(mul_parts);
              // TODO: Move into multiplication:
              exponents_to_mul[*exponent].push_back(std::move(reduced_mul));
            }
            return true;
          } else {
            if (auto [base, exp] = as_base_and_exp(child); base.is_identical_to(collected_term)) {
              // This term is standalone in the sum, so just push one
              exponents_to_mul[exp].push_back(constants::one);
              return true;
            }
          }
          return false;
        });

    container.erase(new_end, container.end());

    // We need to recurse on the remaining terms in the container.
    // Some terms ignored above could still be relevant to the other terms we are collecting over.
    if (collected_terms_.size() > 1 && !container.empty()) {
      scalar_expr remaining_addition_terms =
          collect_visitor{collected_terms_.subspan(1)}.collect_addition_terms(std::move(container));
      container.clear();
      container.push_back(std::move(remaining_addition_terms));
    }

    // Now we're done with the first term, we recurse for the remaining terms.
    for (auto it = exponents_to_mul.begin(); it != exponents_to_mul.end(); ++it) {
      // The term we are collecting, e.g. x, x**2, x**3, etc...
      const scalar_expr collected_pow = power::create(collected_term, it->first);

      // Check if we need to collect remaining terms of our coefficient, otherwise just turn it into
      // an addition. In some cases, the addition might just reduce to a single expression.
      const scalar_expr term_coefficient =
          collected_terms_.size() > 1
              ? collect_visitor{collected_terms_.subspan(1)}.collect_addition_terms(
                    std::move(it->second))
              : addition::from_operands(it->second);

      // Multiply the power by the collected terms: x**2 * (y + pi - 3)
      scalar_expr mul = multiplication::from_two_operands(collected_pow, term_coefficient);
      container.push_back(std::move(mul));
    }
    return addition::from_operands(container);  //  TODO: should be a move
  }

  scalar_expr operator()(const addition& add) {
    // transform all children of the addition:
    addition::container_type children{};
    children.reserve(add.size());
    std::transform(add.begin(), add.end(), std::back_inserter(children),
                   [this](const scalar_expr& x) { return visit(x, *this); });
    return collect_addition_terms(std::move(children));
  }

  template <typename T, typename X, typename = enable_if_not_same_t<T, addition>>
  X operator()(const T& concrete, const X& expr) {
    if constexpr (T::is_leaf_node) {
      return expr;
    } else {
      return concrete.map_children(*this);
    }
  }

 private:
  absl::Span<const scalar_expr> collected_terms_;
};

scalar_expr collect_many(const scalar_expr& arg, const absl::Span<const scalar_expr> terms) {
  if (terms.empty()) {
    return arg;
  }
  for (const scalar_expr& term : terms) {
    if (term.is_type<integer_constant, float_constant, rational_constant>()) {
      throw type_error("Arguments to collect cannot be numeric values. Term = {}", term);
    }
  }
  return visit(arg, collect_visitor{terms});
}

scalar_expr collect(const scalar_expr& arg, const scalar_expr& term) {
  return collect_many(arg, {term});
}

}  // namespace wf
