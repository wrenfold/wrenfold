// Copyright 2023 Gareth Cross
#include <algorithm>

#include "wf/expressions/all_expressions.h"
#include "wf/visit.h"

namespace wf {

// Visitor for collecting terms.
// Can transform x^2*y + x^2*pi + -x * 5 - cos(z) * x --> x^2 * (y + pi) + x * (-5 - cos(z))
struct collect_visitor {
  explicit collect_visitor(const absl::Span<const Expr> terms) : collected_terms_(terms) {}

  Expr operator()(const Expr& x) { return visit(x, *this); }
  compound_expr operator()(const compound_expr& x) { return map_compound_expressions(x, *this); }

  template <typename T>
  auto recurse(const T& op) {
    return op.map_children(*this);
  }

  Expr collect_addition_terms(addition::container_type&& container) const {
    // iterate over the terms we want to search for:
    const Expr& collected_term = collected_terms_.front();

    // Mapping from exponents of `collected_term` to the values multiplied by them.
    // For example, if given: x**2 * 3 + x*5 - x**2 * pi
    // This map would look like:
    //  2 --> [3]
    //  1 --> [5, pi]
    std::unordered_map<Expr, addition::container_type, hash_struct<Expr>, is_identical_struct<Expr>>
        exponents_to_mul{};

    const auto new_end = std::remove_if(container.begin(), container.end(), [&](const Expr& child) {
      if (const multiplication* mul = cast_ptr<const multiplication>(child); mul != nullptr) {
        // Look for relevant terms:
        std::optional<Expr> exponent;
        const auto it = std::find_if(mul->begin(), mul->end(), [&](const Expr& mul_term) {
          auto [base, exp] = as_base_and_exp(mul_term);
          if (base.is_identical_to(collected_term)) {
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
        // So for example, if we had found `x**5 * pi * y`, we'd insert `pi` and `y` into the table.
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
        auto [base, exp] = as_base_and_exp(child);
        if (base.is_identical_to(collected_term)) {
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
      Expr remaining_addition_terms =
          collect_visitor{collected_terms_.subspan(1)}.collect_addition_terms(std::move(container));
      container.clear();
      container.push_back(std::move(remaining_addition_terms));
    }

    // Now we're done with the first term, we recurse for the remaining terms.
    for (auto it = exponents_to_mul.begin(); it != exponents_to_mul.end(); ++it) {
      // The term we are collecting, e.g. x, x**2, x**3, etc...
      Expr collected_pow = power::create(collected_term, it->first);

      // Check if we need to collect remaining terms of our coefficient, otherwise just turn it into
      // an addition. In some cases, the addition might just reduce to a single expression.
      Expr term_coefficient =
          collected_terms_.size() > 1
              ? collect_visitor{collected_terms_.subspan(1)}.collect_addition_terms(
                    std::move(it->second))
              : addition::from_operands(it->second);

      // Multiply the power by the collected terms: x**2 * (y + pi - 3)
      Expr mul =
          multiplication::from_operands({std::move(collected_pow), std::move(term_coefficient)});
      container.push_back(std::move(mul));
    }
    return addition::from_operands(container);  //  TODO: should be a move
  }

  Expr operator()(const addition& add) {
    // transform all children of the addition:
    addition::container_type children{};
    children.reserve(add.size());
    std::transform(add.begin(), add.end(), std::back_inserter(children),
                   [this](const Expr& x) { return visit(x, *this); });
    return collect_addition_terms(std::move(children));
  }

  Expr operator()(const compound_expression_element& el) { return el.map_children(*this); }

  Expr operator()(const multiplication& mul, const Expr&) { return recurse(mul); }
  Expr operator()(const function& f) { return recurse(f); }
  Expr operator()(const power& pow) { return recurse(pow); }
  Expr operator()(const cast_bool& cast) { return recurse(cast); }
  Expr operator()(const conditional& conditional) { return recurse(conditional); }
  Expr operator()(const symbolic_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const derivative& diff, const Expr&) { return recurse(diff); }
  Expr operator()(const complex_infinity&, const Expr& arg) const { return arg; }
  Expr operator()(const integer_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const float_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const rational_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const relational& relation) { return recurse(relation); }
  Expr operator()(const undefined&) const { return constants::undefined; }
  Expr operator()(const variable&, const Expr& arg) const { return arg; }

 private:
  absl::Span<const Expr> collected_terms_;
};

Expr collect_many(const Expr& arg, const absl::Span<const Expr> terms) {
  if (terms.empty()) {
    return arg;
  }
  for (const Expr& term : terms) {
    if (term.is_type<integer_constant, float_constant, rational_constant>()) {
      throw type_error("Arguments to collect cannot be numeric values. Term = {}", term);
    }
  }
  return visit(arg, collect_visitor{terms});
}

Expr collect(const Expr& arg, const Expr& term) { return collect_many(arg, {term}); }

}  // namespace wf
