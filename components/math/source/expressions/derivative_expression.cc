// Copyright 2023 Gareth Cross
#include "expressions/derivative_expression.h"

#include "constants.h"

namespace math {

Expr Derivative::Create(Expr differentiand, Expr arg, int order) {
  ASSERT_GREATER_OR_EQ(order, 1, "Order of the derivative must >= 1");

  if (!arg.Is<Variable, FunctionArgument>()) {
    throw TypeError("Derivatives can only be taken with respect to variables. Arg = {}",
                    arg.ToString());
  }

  if (const Derivative* d = CastPtr<Derivative>(differentiand);
      d != nullptr && d->Arg().IsIdenticalTo(arg)) {
    // We are just increasing the order of a derivative taken with respect to the same variable.
    // dD(f(x), x, n)/dx = D(f(x), x, n + 1)
    return MakeExpr<Derivative>(d->Differentiand(), std::move(arg), d->order_ + order);
  }
  // We don't really do extra simplification here, that already happens in the derivative visitor
  // in `derivative.cc`.
  // dD(f(y), y, n)/dx = D(D(f(y), y, n), x, 1)
  return MakeExpr<Derivative>(std::move(differentiand), std::move(arg), order);
}

}  // namespace math
