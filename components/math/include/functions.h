// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

// User facing math functions that operate on `Expr` go in this file.
namespace math {

// Natural log.
Expr log(const Expr& arg);

// Power.
Expr pow(const Expr& b, const Expr& e);

// Cosine.
Expr cos(const Expr& arg);

// Sine.
Expr sin(const Expr& arg);

// Tangent.
Expr tan(const Expr& arg);

// Inverse cosine.
Expr acos(const Expr& arg);

// Inverse sine.
Expr asin(const Expr& arg);

// Inverse tangent.
Expr atan(const Expr& arg);

// 2-argument inverse tangent.
Expr atan2(const Expr& y, const Expr& x);

// Square root.
Expr sqrt(const Expr& arg);

// Absolute value function.
Expr abs(const Expr& arg);

// Signum function.
Expr signum(const Expr& arg);

// Maximum of two values. Equivalent to: (a < b) ? b : a
Expr max(const Expr& a, const Expr& b);

// Minimum of two values. Equivalent to: (b < a) ? b : a
Expr min(const Expr& a, const Expr& b);

// Conditional/ternary expression.
Expr where(const Expr& condition, const Expr& if_true, const Expr& if_false);

// Conditional over a matrix w/ scalar condition.
MatrixExpr where(const Expr& condition, const MatrixExpr& if_true, const MatrixExpr& if_false);

// Cast the provided expression from a boolean to an integer.
Expr cast_int_from_bool(const Expr& bool_expression);

}  // namespace math
