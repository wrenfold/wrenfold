// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/derivative_expression.h"

#include "wf/constants.h"

namespace wf {

scalar_expr derivative::create(scalar_expr function, scalar_expr arg, int order) {
  WF_ASSERT_GE(order, 1, "Order of the derivative must >= 1");

  if (!arg.is_type<variable>()) {
    throw type_error("Derivatives can only be taken with respect to variables. Arg = {}",
                     arg.to_string());
  }

  if (const derivative* d = get_if<const derivative>(function);
      d != nullptr && d->argument().is_identical_to(arg)) {
    // We are just increasing the order of a derivative taken with respect to the same variable.
    // dD(f(x), x, n)/dx = D(f(x), x, n + 1)
    return make_expr<derivative>(d->differentiand(), std::move(arg), d->order_ + order);
  }
  // dD(f(y), y, n)/dx = D(D(f(y), y, n), x, 1)
  return make_expr<derivative>(std::move(function), std::move(arg), order);
}

}  // namespace wf
