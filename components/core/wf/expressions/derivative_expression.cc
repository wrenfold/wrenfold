// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/derivative_expression.h"

#include "wf/expressions/function_expressions.h"
#include "wf/expressions/variable.h"

namespace wf {

scalar_expr derivative::create(scalar_expr function, scalar_expr arg, int order) {
  if (order <= 0) {
    throw invalid_argument_error("Order of the derivative must be >= 1");
  }

  if (!arg.is_type<variable, unique_variable, function_argument_variable,
                   symbolic_function_invocation>()) {
    throw type_error(
        "Derivatives can only be taken with respect to on of: [{}, {}, {}, {}]. Argument is of "
        "type `{}`: {}",
        variable::name_str, unique_variable::name_str, function_argument_variable::name_str,
        symbolic_function_invocation::name_str, arg.type_name(), arg.to_string());
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
