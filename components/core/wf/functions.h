// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"

// User facing math functions that operate on `scalar_expr` go in this file.
namespace wf {

// Natural log.
scalar_expr log(const scalar_expr& arg);

// Power.
scalar_expr pow(const scalar_expr& b, const scalar_expr& e);

// Cosine.
scalar_expr cos(const scalar_expr& arg);

// Sine.
scalar_expr sin(const scalar_expr& arg);

// Tangent.
scalar_expr tan(const scalar_expr& arg);

// Inverse cosine.
scalar_expr acos(const scalar_expr& arg);

// Inverse sine.
scalar_expr asin(const scalar_expr& arg);

// Inverse tangent.
scalar_expr atan(const scalar_expr& arg);

// Hyperbolic cosine.
scalar_expr cosh(const scalar_expr& arg);

// Hyperbolic sine.
scalar_expr sinh(const scalar_expr& arg);

// Hyperbolic tangent.
scalar_expr tanh(const scalar_expr& arg);

// Inverse hyperbolic cosine.
scalar_expr acosh(const scalar_expr& arg);

// Inverse hyperbolic sine.
scalar_expr asinh(const scalar_expr& arg);

// Inverse hyperbolic tangent.
scalar_expr atanh(const scalar_expr& arg);

// 2-argument inverse tangent.
scalar_expr atan2(const scalar_expr& y, const scalar_expr& x);

// Square root.
scalar_expr sqrt(const scalar_expr& arg);

// Absolute value function.
scalar_expr abs(const scalar_expr& arg);

// Signum function.
scalar_expr signum(const scalar_expr& arg);

// Floor to integer.
scalar_expr floor(const scalar_expr& arg);

// Maximum of two values. Equivalent to: (a < b) ? b : a
scalar_expr max(const scalar_expr& a, const scalar_expr& b);

// Minimum of two values. Equivalent to: (b < a) ? b : a
scalar_expr min(const scalar_expr& a, const scalar_expr& b);

// Conditional/ternary expression.
scalar_expr where(const boolean_expr& condition, const scalar_expr& if_true,
                  const scalar_expr& if_false);

// Conditional over a matrix w/ scalar condition.
matrix_expr where(const boolean_expr& condition, const matrix_expr& if_true,
                  const matrix_expr& if_false);

// Iverson bracket: Cast the provided expression from a boolean to an integer.
// Evaluates to 1 if bool_expression is true, and 0 if bool_expression is false.
scalar_expr iverson(const boolean_expr& bool_expression);

// Get the real and imaginary parts of an expression.
// Implemented in real_imaginary.cc
std::tuple<scalar_expr, scalar_expr> real_imag(const scalar_expr& arg);

}  // namespace wf
