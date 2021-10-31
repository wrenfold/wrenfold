#pragma once
#include "expr.h"

namespace math {

// Natural log.
Expr log(const Expr& x);

// Power.
Expr pow(const Expr& x, const Expr& y);

}  // namespace math
