// Copyright 2023 Gareth Cross
#include "wf/evaluate.h"
#include "wf/expression.h"
#include "wf/visitor_impl.h"

namespace wf {

template <typename T, typename>
Expr evaluate_visitor::operator()(const T& input_typed, const Expr& input) {
  if constexpr (type_list_contains_v<T, derivative, complex_infinity>) {
    throw type_error("Cannot call eval on expression of type: {}", T::name_str);
  } else if constexpr (T::is_leaf_node) {
    return input;
  } else {
    return input_typed.map_children([this](const Expr& expr) { return apply(expr); });
  }
}

Expr evaluate_visitor::operator()(const integer_constant& x) const {
  return float_constant::create(static_cast<float_constant>(x));
}

Expr evaluate_visitor::operator()(const rational_constant& x) const {
  return float_constant::create(static_cast<float_constant>(x));
}

Expr evaluate_visitor::operator()(const symbolic_constant& c) const {
  const auto c_enum = c.name();
  const double value = double_from_symbolic_constant(c_enum);
  WF_ASSERT(!std::isnan(value), "Invalid symbolic constant: {}",
            string_from_symbolic_constant(c_enum));
  return float_constant::create(value);
}

Expr evaluate_visitor::apply(const Expr& input) {
  auto it = cache_.find(input);
  if (it != cache_.end()) {
    return it->second;
  }
  Expr result = visit_with_expr(input, *this);
  cache_.emplace(input, result);
  return result;
}

Expr evaluate(const Expr& arg) { return evaluate_visitor{}.apply(arg); }

}  // namespace wf
