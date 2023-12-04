// Copyright 2023 Gareth Cross
#include "wf/expressions/derivative_expression.h"

#include "wf/constants.h"

namespace math {

Expr Derivative::create(Expr differentiand, Expr arg, int order) {
  WF_ASSERT_GREATER_OR_EQ(order, 1, "Order of the derivative must >= 1");

  if (!arg.is_type<Variable>()) {
    throw type_error("Derivatives can only be taken with respect to variables. Arg = {}",
                     arg.to_string());
  }

  if (const Derivative* d = cast_ptr<Derivative>(differentiand);
      d != nullptr && d->argument().is_identical_to(arg)) {
    // We are just increasing the order of a derivative taken with respect to the same variable.
    // dD(f(x), x, n)/dx = D(f(x), x, n + 1)
    return make_expr<Derivative>(d->differentiand(), std::move(arg), d->order_ + order);
  }
  // dD(f(y), y, n)/dx = D(D(f(y), y, n), x, 1)
  return make_expr<Derivative>(std::move(differentiand), std::move(arg), order);
}

}  // namespace math
