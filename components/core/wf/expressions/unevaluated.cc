// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/unevaluated.h"

namespace wf {

scalar_expr unevaluated::create(scalar_expr contents) {
  // Don't nest parentheses - this would be meaningless.
  if (const unevaluated* p = get_if<const unevaluated>(contents); p != nullptr) {
    return contents;
  } else {
    return make_expr<unevaluated>(std::move(contents));
  }
}

scalar_expr make_unevaluated(scalar_expr expr) { return unevaluated::create(std::move(expr)); }

}  // namespace wf
