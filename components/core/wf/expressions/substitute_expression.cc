// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expressions/substitute_expression.h"

namespace wf {

scalar_expr substitution::create(scalar_expr input, scalar_expr target, scalar_expr replacement) {
  if (input.is_type<integer_constant, float_constant, rational_constant>()) {
    // Constants cannot be replaced, so we can return this directly.
    return input;
  }
  if (target.is_type<integer_constant, float_constant, rational_constant>()) {
    // Don't allow replacing integers or floats.
    throw type_error("Cannot perform a substitution with target expression: {}", target);
  }
  if (target.is_identical_to(replacement)) {
    return input;
  }
  // TODO: For simple substitutions (like variables), check if `target` appears in `input`.
  return make_expr<substitution>(std::move(input), std::move(target), std::move(replacement));
}

}  // namespace wf
