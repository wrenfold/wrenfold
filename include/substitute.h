// Copyright 2023 Gareth Cross
#include "expression.h"
#include "expression_impl.h"

namespace math {

// Replace instances of `target` w/ `replacement` in the input expression tree `input`.
Expr Substitute(const Expr& input, const Expr& target, const Expr& replacement);

}  // namespace math
