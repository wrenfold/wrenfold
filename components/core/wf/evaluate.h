// Copyright 2023 Gareth Cross
#pragma once
#include "wf/compound_expression.h"
#include "wf/expression.h"
#include "wf/expression_cache.h"
#include "wf/matrix_expression.h"

namespace wf {

// Convert expressions to `float_constant` values.
class evaluate_visitor {
 public:
  template <typename T, typename X,
            typename = enable_if_does_not_contain_type_t<T, integer_constant, rational_constant,
                                                         symbolic_constant>>
  X operator()(const T& input_typed, const X& input);

  scalar_expr operator()(const integer_constant& x) const;
  scalar_expr operator()(const rational_constant& x) const;
  scalar_expr operator()(const symbolic_constant& c) const;

  // Visit and cache the result.
  scalar_expr operator()(const scalar_expr& input);
  compound_expr operator()(const compound_expr& input);
  boolean_expr operator()(const boolean_expr& input);
  matrix_expr operator()(const matrix_expr& input);

 private:
  wf::expression_cache<> cache_;
};

}  // namespace wf
