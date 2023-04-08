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

// Sqrt
Expr sqrt(const Expr& arg);

}  // namespace math
