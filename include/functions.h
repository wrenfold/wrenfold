#pragma once
#include "expr.h"

// User facing math functions that operate on `Expr` go in this file.
namespace math {

// Natural log.
Expr log(const Expr& x);

// Power.
Expr pow(const Expr& x, const Expr& y);

}  // namespace math
