// Copyright 2023 Gareth Cross
#include "wf/expressions/derivative_expression.h"

#include "wf/constants.h"

namespace math {

Expr derivative::create(Expr function, Expr arg, int order) {
  WF_ASSERT_GREATER_OR_EQ(order, 1, "Order of the derivative must >= 1");

  if (!arg.is_type<variable>()) {
    throw type_error("Derivatives can only be taken with respect to variables. Arg = {}",
                     arg.to_string());
  }

  if (const derivative* d = cast_ptr<derivative>(function);
      d != nullptr && d->argument().is_identical_to(arg)) {
    // We are just increasing the order of a derivative taken with respect to the same variable.
    // dD(f(x), x, n)/dx = D(f(x), x, n + 1)
    return make_expr<derivative>(d->differentiand(), std::move(arg), d->order_ + order);
  }
  // dD(f(y), y, n)/dx = D(D(f(y), y, n), x, 1)
  return make_expr<derivative>(std::move(function), std::move(arg), order);
}

}  // namespace math
