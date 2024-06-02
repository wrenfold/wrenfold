// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/boolean_expression.h"

#include "wf/constants.h"
#include "wf/expression_visitor.h"
#include "wf/plain_formatter.h"
#include "wf/substitute.h"
#include "wf/tree_formatter.h"

namespace wf {

std::string boolean_expr::to_string() const {
  plain_formatter formatter{};
  formatter(*this);
  return formatter.take_output();
}

std::string boolean_expr::to_expression_tree_string() const {
  tree_formatter_visitor formatter{};
  formatter(*this);
  return formatter.take_output();
}

boolean_expr boolean_expr::subs(const scalar_expr& a, const scalar_expr& b) const {
  return wf::substitute(*this, a, b);
}

boolean_expr boolean_expr::construct_implicit(const bool value) {
  // Return an existing constant instead of allocating a new one:
  if (value) {
    return constants::boolean_true;
  } else {
    return constants::boolean_false;
  }
}

relative_order order_struct<boolean_expr>::operator()(const boolean_expr& a,
                                                      const boolean_expr& b) const {
  if (a.type_index() < b.type_index()) {
    return relative_order::less_than;
  } else if (a.type_index() > b.type_index()) {
    return relative_order::greater_than;
  }
  return visit(a, [&b](const auto& a_typed) -> relative_order {
    using Ta = std::decay_t<decltype(a_typed)>;
    static_assert(is_orderable_v<Ta>, "Type does not implement order_struct.");
    return order_struct<Ta>{}(a_typed, get_unchecked<const Ta>(b));
  });
}

}  // namespace wf
