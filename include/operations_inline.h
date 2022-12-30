#pragma once
#include <type_traits>

#include "assertions.h"
#include "expression.h"
#include "operation_types.h"
#include "power.h"
#include "unary_operations.h"
#include "visitor_impl.h"

namespace math {

inline Expr operator*(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, b);
}

inline Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

inline Expr operator-(const Expr& a, const Expr& b) { return a + Negation::Create(b); }

inline Expr operator/(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, Power::Create(b, -1));
}

}  // namespace math
