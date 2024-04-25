// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/evaluate.h"
#include "wf/expression.h"
#include "wf/expression_visitor.h"

namespace wf {

template <typename T, typename X, typename>
X evaluate_visitor::operator()(const T& input_typed, const X& input) {
  if constexpr (type_list_contains_v<T, derivative, complex_infinity>) {
    throw type_error("Cannot call eval on expression of type: {}", T::name_str);
  } else if constexpr (T::is_leaf_node) {
    return input;
  } else {
    return input_typed.map_children(*this);
  }
}

scalar_expr evaluate_visitor::operator()(const integer_constant& x) const {
  return scalar_expr(static_cast<float_constant>(x));
}

scalar_expr evaluate_visitor::operator()(const rational_constant& x) const {
  return scalar_expr(static_cast<float_constant>(x));
}

scalar_expr evaluate_visitor::operator()(const symbolic_constant& c) const {
  const auto c_enum = c.name();
  return scalar_expr{double_from_symbolic_constant(c_enum)};
}

scalar_expr evaluate_visitor::operator()(const scalar_expr& input) {
  return cache_.get_or_insert(input, [this](const scalar_expr& x) { return visit(x, *this); });
}

compound_expr evaluate_visitor::operator()(const compound_expr& input) {
  return cache_.get_or_insert(
      input, [this](const compound_expr& x) { return map_compound_expressions(x, *this); });
}

boolean_expr evaluate_visitor::operator()(const boolean_expr& input) {
  return cache_.get_or_insert(input, [this](const auto& x) { return visit(x, *this); });
}

matrix_expr evaluate_visitor::operator()(const matrix_expr& input) {
  return cache_.get_or_insert(
      input, [this](const matrix_expr& x) { return map_matrix_expression(x, *this); });
}

scalar_expr evaluate(const scalar_expr& arg) { return evaluate_visitor{}(arg); }

}  // namespace wf
