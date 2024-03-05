// Copyright 2023 Gareth Cross
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
  const double value = double_from_symbolic_constant(c_enum);
  WF_ASSERT(!std::isnan(value), "Invalid symbolic constant: {}",
            string_from_symbolic_constant(c_enum));
  return scalar_expr(value);
}

scalar_expr evaluate_visitor::operator()(const scalar_expr& input) {
  if (auto it = cache_.find(input); it != cache_.end()) {
    return it->second;
  }
  scalar_expr result = visit(input, *this);
  cache_.emplace(input, result);
  return result;
}

compound_expr evaluate_visitor::operator()(const compound_expr& input) {
  if (auto it = compound_cache_.find(input); it != compound_cache_.end()) {
    return it->second;
  }
  compound_expr result = map_compound_expressions(input, *this);
  compound_cache_.emplace(input, result);
  return result;
}

boolean_expr evaluate_visitor::operator()(const boolean_expr& input) { return visit(input, *this); }

scalar_expr evaluate(const scalar_expr& arg) { return evaluate_visitor{}(arg); }

}  // namespace wf
