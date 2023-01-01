#pragma once
#include <type_traits>

#include "assertions.h"
#include "expression.h"
#include "expressions/addition.h"
#include "expressions/function_expressions.h"
#include "expressions/multiplication.h"
#include "expressions/power.h"
#include "visitor_impl.h"

namespace math {

inline Expr operator*(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, b);
}

inline Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

inline Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::FromTwoOperands(Constants::NegativeOne, b);
}

inline Expr operator/(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, Power::Create(b, Constants::NegativeOne));
}

}  // namespace math
