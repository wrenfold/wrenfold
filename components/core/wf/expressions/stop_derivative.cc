// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/stop_derivative.h"

namespace wf {

scalar_expr stop_derivative::create(scalar_expr arg) {
  if (arg.is_type<stop_derivative>()) {
    return arg;
  } else {
    return scalar_expr{std::in_place_type_t<stop_derivative>{}, std::move(arg)};
  }
}

scalar_expr stop_diff(scalar_expr arg) { return stop_derivative::create(std::move(arg)); }

}  // namespace wf
