// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <pybind11/pybind11.h>

#include "wf/expression.h"

namespace wf {

// Visitor used in the wrapper to retrieve arguments to an expression.
class args_visitor {
 public:
  using tuple = pybind11::tuple;

  tuple operator()(const scalar_expr& input) const;
  tuple operator()(const boolean_expr& input) const;

  template <typename T, typename = enable_if_does_not_contain_type_t<T, scalar_expr, boolean_expr>>
  tuple operator()(const T&) const;
};

}  // namespace wf
