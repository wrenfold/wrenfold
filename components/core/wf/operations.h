// Copyright 2023 Gareth Cross
#pragma once
#include <optional>

#include "wf/absl_imports.h"
#include "wf/enumerations.h"

namespace wf {
class Expr;
class matrix_expr;

// Replace instances of `target` w/ `replacement` in the input expression tree `input`.
// Implemented in substitute.cc
Expr substitute(const Expr& input, const Expr& target, const Expr& replacement);

// Replace all the [target, replacement] pairs in the input expression tree `input`.
// Every `target` must be a variable.
Expr substitute_variables(const Expr& input, absl::Span<const std::tuple<Expr, Expr>> pairs);

// Replace all the [target, replacement] pairs in the input matrix expression tree `input`.
// Every `target` must be a variable.
matrix_expr substitute_variables(const matrix_expr& input,
                                 absl::Span<const std::tuple<Expr, Expr>> pairs);

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
Expr diff(const Expr& function, const Expr& var, int reps, non_differentiable_behavior behavior);

// Compute the jacobian of the vector `functions` wrt the variables in `vars`.
// If `functions` has length `N` and `vars` length `M`, the result will be NxM.
matrix_expr jacobian(absl::Span<const Expr> functions, absl::Span<const Expr> vars,
                     non_differentiable_behavior behavior);

// Distribute over multiplications in the input expression.
// Expands terms like: (a + b) * (c * d) --> a*c + a*d + b*c + b*d
// Implemented in distribute.cc
Expr distribute(const Expr& arg);

// Collect powers of the specified terms.
// Terms are prioritized by their order in `terms`.
// Implemented in collect.cc
Expr collect_many(const Expr& arg, absl::Span<const Expr> terms);

// Version of `collect_many` that accepts a single term.
Expr collect(const Expr& arg, const Expr& term);

// Evaluate to a number. If the result is a matrix, returns a matrix expression of numbers.
// Implemented in evaluate.cc
Expr evaluate(const Expr& arg);

// Take the limit of `f_of_x` as `x` goes to 0 from the right.
// The expression `x` must be a variable.
// Implemented in limit.cc
// This feature is considered unstable.
std::optional<Expr> limit(const Expr& f_of_x, const Expr& x);

// Take the limit of matrix-valued function `f_of_x` as `x` goes to 0 from the right.
// The expression `x` must be a variable.
// This feature is considered unstable.
std::optional<matrix_expr> limit(const matrix_expr& f_of_x, const Expr& x);

// Determine what set of numbers an expression belongs to.
// Returns `NumberSet::unknown` if the set cannot be determined.
number_set determine_numeric_set(const Expr& x);

}  // namespace wf
