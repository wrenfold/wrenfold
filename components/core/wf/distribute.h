// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
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

  scalar_expr operator()(const multiplication& mul);
  scalar_expr operator()(const power& pow);

  template <typename T, typename X,
            typename = enable_if_does_not_contain_type_t<T, type_list<power, multiplication>>>
  X operator()(const T& concrete, const X& expr);

 private:
  // Expand base^power.
  static scalar_expr distribute_power(scalar_expr base, std::size_t power);

  // Expand the multiplication `a * b`. If either of `a` or `b` is an addition, we distribute terms.
  static scalar_expr distribute_multiplied_terms(const scalar_expr& a, const scalar_expr& b);

  template <typename Container>
  scalar_expr distribute_multiplied_terms(const Container& multiplied_terms);

  wf::expression_cache cache_;
};

// Distribute over multiplications in the input expression.
// Expands terms like: (a + b) * (c * d) --> a*c + a*d + b*c + b*d
scalar_expr distribute(const scalar_expr& arg);

}  // namespace wf
