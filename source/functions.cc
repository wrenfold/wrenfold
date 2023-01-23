#include "functions.h"

#include <unordered_map>

#include "expressions/all_expressions.h"

namespace math {

Expr log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  if (IsOne(x)) {
    return Constants::Zero;
  }
  // TODO: Check for negative values.
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Log, x);
}

Expr pow(const Expr& x, const Expr& y) { return Power::Create(x, y); }

Expr cos(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Cos, arg); }

Expr sin(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Sin, arg); }

Expr tan(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Tan, arg); }

Expr acos(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcCos, arg); }

Expr asin(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcSin, arg); }

Expr atan(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcTan, arg); }

Expr sqrt(const Expr& arg) {
  static const Expr one_half = Constants::One / 2_s;
  return Power::Create(arg, one_half);
}

}  // namespace math
