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

// Inverse tangent 2.
Expr atan2(const Expr& y, const Expr& x);

// Sqrt
Expr sqrt(const Expr& arg);

// Conditional/ternary expression.
Expr where(const Expr& condition, const Expr& if_true, const Expr& if_false);

}  // namespace math
