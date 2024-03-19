// Copyright 2023 Gareth Cross
#pragma once
#include "wf/enumerations.h"
#include "wf/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {
class scalar_expr;
class matrix_expr;
class boolean_expr;

// Replace instances of `target` w/ `replacement` in the input expression tree `input`.
// Implemented in substitute.cc
scalar_expr substitute(const scalar_expr& input, const scalar_expr& target,
                       const scalar_expr& replacement);
boolean_expr substitute(const boolean_expr& input, const scalar_expr& target,
                        const scalar_expr& replacement);

// Replace all the [target, replacement] pairs in the input expression tree `input`.
// Every `target` must be a variable.
scalar_expr substitute_variables(const scalar_expr& input,
                                 absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs);

// Replace all the [target, replacement] pairs in the input matrix expression tree `input`.
// Every `target` must be a variable.
matrix_expr substitute_variables(const matrix_expr& input,
                                 absl::Span<const std::tuple<scalar_expr, scalar_expr>> pairs);

// Govern behavior of the derivative visitor when we encounter non-differentiable functions.
// An example would be the round(x) function, which has derivatives:
//  - 0 for integer `x`.
//  - undefined everywhere else.
enum class non_differentiable_behavior {
  // Replace the derivative of non-differentiable functions with a suitable constant.
  // This is not strictly correct mathematically, but is more computationally useful in most cases.
  constant,
  // Insert abstract `derivative` expressions to represent something we don't know how to
  // differentiate: df(x)/dx --> derivative(f(x), x, 1)
  abstract,
};

// Take the derivative of `function` wrt `var`. The derivative is taken `reps` times.
// Implemented in derivative.cc
scalar_expr diff(const scalar_expr& function, const scalar_expr& var, int reps,
                 non_differentiable_behavior behavior);

// Compute the jacobian of the vector `functions` wrt the variables in `vars`.
// If `functions` has length `N` and `vars` length `M`, the result will be NxM.
matrix_expr jacobian(absl::Span<const scalar_expr> functions, absl::Span<const scalar_expr> vars,
                     non_differentiable_behavior behavior);

// Distribute over multiplications in the input expression.
// Expands terms like: (a + b) * (c * d) --> a*c + a*d + b*c + b*d
// Implemented in distribute.cc
scalar_expr distribute(const scalar_expr& arg);

// Collect powers of the specified terms.
// Terms are prioritized by their order in `terms`.
// Implemented in collect.cc
scalar_expr collect_many(const scalar_expr& arg, absl::Span<const scalar_expr> terms);

// Version of `collect_many` that accepts a single term.
scalar_expr collect(const scalar_expr& arg, const scalar_expr& term);

// Evaluate to a number. If the result is a matrix, returns a matrix expression of numbers.
// Implemented in evaluate.cc
scalar_expr evaluate(const scalar_expr& arg);

// Determine what set of numbers an expression belongs to.
// Returns `NumberSet::unknown` if the set cannot be determined.
number_set determine_numeric_set(const scalar_expr& x);

}  // namespace wf
