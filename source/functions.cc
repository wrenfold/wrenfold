#include "functions.h"

#include "expressions/all_expressions.h"

namespace math {

Expr log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  if (IsOne(x)) {
    return Constants::Zero;
  }
  return MakeExpr<UnaryFunction>(UnaryFunctionName::Log, x);
}

Expr pow(const Expr& x, const Expr& y) { return Power::Create(x, y); }

Expr cos(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Cos, arg); }

Expr sin(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Sin, arg); }

Expr tan(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::Tan, arg); }

Expr acos(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcCos, arg); }

Expr asin(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcSin, arg); }

Expr atan(const Expr& arg) { return MakeExpr<UnaryFunction>(UnaryFunctionName::ArcTan, arg); }

}  // namespace math
