// Copyright 2023 Gareth Cross
#include <algorithm>

#include "wf/assertions.h"
#include "wf/expressions/all_expressions.h"
#include "wf/visit.h"

namespace wf {

// Visitor for distributing terms in multiplications:
// (a + b) * (x + y) = a*x + a*y + b*x + b*y
struct distribute_visitor {
  Expr operator()(const Expr& x) const { return visit(x, *this); }

  compound_expr operator()(const compound_expr& x) const {
    return map_compound_expressions(x, *this);
  }

  Expr operator()(const addition& add) const { return add.map_children(&distribute); }

  Expr operator()(const compound_expression_element& el) const { return el.map_children(*this); }

  Expr operator()(const multiplication& mul, const Expr&) const {
    // First distribute all the children of the multiplication:
    std::vector<Expr> children{};
    children.reserve(mul.size());
    std::transform(mul.begin(), mul.end(), std::back_inserter(children),
                   [](const Expr& expr) { return distribute(expr); });

    // Are any of the child expressions additions?
    const std::size_t total_terms = std::accumulate(
        children.begin(), children.end(), static_cast<std::size_t>(1lu),
        [](std::size_t total, const Expr& expr) {
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
    // values. TODO: Make each of these a small vector of Expr, then convert them to multiplication.
    std::vector<Expr> output_terms(total_terms, constants::one);

    std::size_t step = total_terms;
    for (const Expr& expr : children) {
      if (const addition* add = cast_ptr<const addition>(expr); add != nullptr) {
        // For additions, first update the step by dividing by the size of this addition:
        WF_ASSERT_EQUAL(0, step % add->size());
        WF_ASSERT_GREATER_OR_EQ(step / add->size(), 1);
        step /= add->size();
        // Now multiply terms in the addition:
        for (std::size_t out = 0; out < total_terms;) {
          for (const Expr& term : *add) {
            for (std::size_t rep = 0; rep < step; ++rep, ++out) {
              output_terms[out] = output_terms[out] * term;
            }
          }
        }
      } else {
        // Not an addition, multiply by everything:
        for (Expr& output : output_terms) {
          output = output * expr;
        }
      }
    }

    return addition::from_operands(output_terms);
  }

  Expr operator()(const function& f, const Expr&) const { return f.map_children(&distribute); }

  Expr operator()(const power& pow, const Expr&) const {
    // TODO: If base is an addition, and exponent an integer, we should distribute.
    const Expr& a = pow.base();
    const Expr& b = pow.exponent();
    return power::create(distribute(a), distribute(b));
  }

  Expr operator()(const cast_bool& cast) const { return cast.map_children(&distribute); }
  Expr operator()(const conditional& conditional, const Expr&) const {
    return conditional.map_children(&distribute);
  }

  Expr operator()(const symbolic_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const derivative& diff, const Expr&) const {
    return diff.map_children(&distribute);
  }
  Expr operator()(const complex_infinity&, const Expr& arg) const { return arg; }
  Expr operator()(const integer_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const float_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const rational_constant&, const Expr& arg) const { return arg; }
  Expr operator()(const relational& relation, const Expr&) const {
    return relation.map_children(&distribute);
  }
  Expr operator()(const undefined&) const { return constants::undefined; }
  Expr operator()(const variable&, const Expr& arg) const { return arg; }
};

Expr distribute(const Expr& arg) { return visit(arg, distribute_visitor{}); }

}  // namespace wf
