// Copyright 2023 Gareth Cross
#include <algorithm>

#include "wf/assertions.h"
#include "wf/expressions/all_expressions.h"
#include "wf/visit.h"

namespace wf {

// Visitor for distributing terms in multiplications:
// (a + b) * (x + y) = a*x + a*y + b*x + b*y
struct distribute_visitor {
  scalar_expr operator()(const scalar_expr& x) const { return visit(x, *this); }

  compound_expr operator()(const compound_expr& x) const {
    return map_compound_expressions(x, *this);
  }

  scalar_expr operator()(const addition& add) const { return add.map_children(&distribute); }

  scalar_expr operator()(const compound_expression_element& el) const {
    return el.map_children(*this);
  }

  scalar_expr operator()(const multiplication& mul, const scalar_expr&) const {
    // First distribute all the children of the multiplication:
    std::vector<scalar_expr> children{};
    children.reserve(mul.size());
    std::transform(mul.begin(), mul.end(), std::back_inserter(children),
                   [](const scalar_expr& expr) { return distribute(expr); });

    // Are any of the child expressions additions?
    const std::size_t total_terms = std::accumulate(
        children.begin(), children.end(), static_cast<std::size_t>(1lu),
        [](std::size_t total, const scalar_expr& expr) {
          if (const addition* const add = cast_ptr<const addition>(expr); add != nullptr) {
            total *= add->size();
          }
          return total;
        });

    // If the total terms is > 1, we have an addition to distribute over.
    const bool contains_additions = total_terms > 1;
    if (!contains_additions) {
      // If there are no additions, just create a new multiplication:
      // TODO: If there are no additions, and no children were altered, we could avoid this step.
      return multiplication::from_operands(children);
    }

    // Otherwise, we need to expand all the terms. This multiplication will become an addition of
    // multiplications. For each addition, we need to distribute its terms over the remaining
    // values. TODO: Make each of these a small vector of scalar_expr, then convert them to
    // multiplication.
    std::vector<scalar_expr> output_terms(total_terms, constants::one);

    std::size_t step = total_terms;
    for (const scalar_expr& expr : children) {
      if (const addition* add = cast_ptr<const addition>(expr); add != nullptr) {
        // For additions, first update the step by dividing by the size of this addition:
        WF_ASSERT_EQUAL(0, step % add->size());
        WF_ASSERT_GREATER_OR_EQ(step / add->size(), 1);
        step /= add->size();
        // Now multiply terms in the addition:
        for (std::size_t out = 0; out < total_terms;) {
          for (const scalar_expr& term : *add) {
            for (std::size_t rep = 0; rep < step; ++rep, ++out) {
              output_terms[out] = output_terms[out] * term;
            }
          }
        }
      } else {
        // Not an addition, multiply by everything:
        for (scalar_expr& output : output_terms) {
          output = output * expr;
        }
      }
    }

    return addition::from_operands(output_terms);
  }

  scalar_expr operator()(const function& f, const scalar_expr&) const {
    return f.map_children(&distribute);
  }

  scalar_expr operator()(const power& pow, const scalar_expr&) const {
    // TODO: If base is an addition, and exponent an integer, we should distribute.
    const scalar_expr& a = pow.base();
    const scalar_expr& b = pow.exponent();
    return power::create(distribute(a), distribute(b));
  }

  scalar_expr operator()(const cast_bool& cast) const { return cast.map_children(&distribute); }
  scalar_expr operator()(const conditional& conditional, const scalar_expr&) const {
    return conditional.map_children(&distribute);
  }

  scalar_expr operator()(const symbolic_constant&, const scalar_expr& arg) const { return arg; }
  scalar_expr operator()(const derivative& diff, const scalar_expr&) const {
    return diff.map_children(&distribute);
  }
  scalar_expr operator()(const complex_infinity&, const scalar_expr& arg) const { return arg; }
  scalar_expr operator()(const integer_constant&, const scalar_expr& arg) const { return arg; }
  scalar_expr operator()(const float_constant&, const scalar_expr& arg) const { return arg; }
  scalar_expr operator()(const rational_constant&, const scalar_expr& arg) const { return arg; }
  scalar_expr operator()(const relational& relation, const scalar_expr&) const {
    return relation.map_children(&distribute);
  }
  scalar_expr operator()(const undefined&) const { return constants::undefined; }
  scalar_expr operator()(const variable&, const scalar_expr& arg) const { return arg; }
};

scalar_expr distribute(const scalar_expr& arg) { return visit(arg, distribute_visitor{}); }

}  // namespace wf
