// Copyright 2024 Gareth Cross
#pragma once
#include "wf/expression_cache.h"

namespace wf {

// Visitor for distributing terms in multiplications:
// (a + b) * (x + y) = a*x + a*y + b*x + b*y
// (a + b)^2 = a^2 + 2*a*b + b^2
class distribute_visitor {
 public:
  scalar_expr operator()(const scalar_expr& input);
  compound_expr operator()(const compound_expr& input);
  boolean_expr operator()(const boolean_expr& input);
  matrix_expr operator()(const matrix_expr& input);

  scalar_expr operator()(const addition& add);
  boolean_expr operator()(const boolean_constant&, const boolean_expr& arg) const;
  scalar_expr operator()(const compound_expression_element& el);
  scalar_expr operator()(const complex_infinity&) const;
  scalar_expr operator()(const conditional& conditional);
  scalar_expr operator()(const derivative& diff);
  scalar_expr operator()(const imaginary_unit&) const;
  scalar_expr operator()(const integer_constant&, const scalar_expr& arg) const;
  scalar_expr operator()(const iverson_bracket& cast);
  scalar_expr operator()(const float_constant&, const scalar_expr& arg) const;
  scalar_expr operator()(const function& f);
  scalar_expr operator()(const multiplication& mul);
  scalar_expr operator()(const power& pow);
  scalar_expr operator()(const rational_constant&, const scalar_expr& arg) const;
  boolean_expr operator()(const relational& relation);
  scalar_expr operator()(const symbolic_constant&, const scalar_expr& arg) const;
  scalar_expr operator()(const undefined&) const;
  scalar_expr operator()(const variable&, const scalar_expr& arg) const;

 private:
  // Expand base^power.
  static scalar_expr distribute_power(scalar_expr base, std::size_t power);

  // Expand the multiplication `a * b`. If either of `a` or `b` is an addition, we distribute terms.
  static scalar_expr distribute_multiplied_terms(const scalar_expr& a, const scalar_expr& b);

  template <typename Container>
  scalar_expr distribute_multiplied_terms(const Container& multiplied_terms);

  wf::expression_cache<> cache_;
};

}  // namespace wf
