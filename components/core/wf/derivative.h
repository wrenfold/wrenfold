// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_map>

#include "expressions/function_expressions.h"
#include "wf/enumerations.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace wf {

// Visitor that takes the derivative of an input expression.
class derivative_visitor {
 public:
  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  derivative_visitor(const scalar_expr& argument, non_differentiable_behavior behavior);

  // Apply this visitor to the specified expression.
  scalar_expr operator()(const scalar_expr& expression);

  scalar_expr operator()(const addition& add);
  scalar_expr operator()(const compound_expression_element&, const scalar_expr& expr) const;
  scalar_expr operator()(const conditional& cond);
  scalar_expr operator()(const symbolic_constant&) const;
  scalar_expr operator()(const derivative& derivative,
                         const scalar_expr& derivative_abstract) const;
  scalar_expr operator()(const multiplication& mul);
  scalar_expr operator()(const built_in_function_invocation& func,
                         const scalar_expr& func_abstract);
  scalar_expr operator()(const complex_infinity&) const;
  scalar_expr operator()(const imaginary_unit&) const;
  scalar_expr operator()(const integer_constant&) const;
  scalar_expr operator()(const iverson_bracket&, const scalar_expr& arg) const;
  scalar_expr operator()(const float_constant&) const;
  scalar_expr operator()(const power& pow);
  scalar_expr operator()(const rational_constant&) const;
  scalar_expr operator()(const symbolic_function_invocation& func);
  scalar_expr operator()(const undefined&) const;
  scalar_expr operator()(const unevaluated& u);
  scalar_expr operator()(const variable& var) const;

 private:
  // Argument we differentiate with respect to.
  const scalar_expr& argument_;

  // See `non_differentiable_behavior` - how we handle non-differentiable functions.
  non_differentiable_behavior non_diff_behavior_;

  // Cache of f(x) --> df(x)/dx.
  std::unordered_map<scalar_expr, scalar_expr, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      cache_;
};

// Take the derivative of `function` wrt `var`. The derivative is taken `reps` times.
scalar_expr diff(const scalar_expr& function, const scalar_expr& var, int reps,
                 non_differentiable_behavior behavior);

// Compute the jacobian of the vector `functions` wrt the variables in `vars`.
// If `functions` has length `N` and `vars` length `M`, the result will be NxM.
matrix_expr jacobian(absl::Span<const scalar_expr> functions, absl::Span<const scalar_expr> vars,
                     non_differentiable_behavior behavior);

}  // namespace wf
