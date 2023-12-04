// Copyright 2023 Gareth Cross
#include "wf/evaluate.h"
#include "wf/expression.h"
#include "wf/visitor_impl.h"

namespace math {

template <typename T, typename>
Expr evaluate_visitor::operator()(const T& input_typed, const Expr& input) {
  if constexpr (type_list_contains_type_v<T, Derivative, Infinity>) {
    throw type_error("Cannot call eval on expression of type: {}", T::NameStr);
  } else if constexpr (T::IsLeafNode) {
    return input;
  } else {
    return input_typed.map_children([this](const Expr& expr) { return apply(expr); });
  }
}

Expr evaluate_visitor::operator()(const Integer& x) const {
  return Float::create(static_cast<Float>(x));
}

Expr evaluate_visitor::operator()(const Rational& x) const {
  return Float::create(static_cast<Float>(x));
}

Expr evaluate_visitor::operator()(const Constant& c) const {
  const auto c_enum = c.name();
  const double value = double_from_symbolic_constant(c_enum);
  WF_ASSERT(!std::isnan(value), "Invalid symbolic constant: {}",
            string_from_symbolic_constant(c_enum));
  return Float::create(value);
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

}  // namespace math
