// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/compound_expression.h"

#include "wf/expression_visitor.h"
#include "wf/plain_formatter.h"
#include "wf/tree_formatter.h"
#include "wf/utility/index_range.h"

namespace wf {

std::string compound_expr::to_string() const { return plain_formatter::convert(*this); }

std::string compound_expr::to_expression_tree_string() const {
  tree_formatter_visitor formatter{};
  formatter(*this);
  return formatter.take_output();
}

std::vector<scalar_expr> create_expression_elements(const compound_expr& provenance,
                                                    const std::size_t num) {
  std::vector<scalar_expr> elements{};
  elements.reserve(num);
  for (const std::size_t index : make_range(num)) {
    elements.emplace_back(compound_expression_element::create(provenance, index));
  }
  return elements;
}

compound_expr create_custom_type_argument(const custom_type& type, const std::string_view name) {
  return compound_expr{std::in_place_type_t<custom_type_argument>{}, std::string(name), type};
}

compound_expr create_custom_type_construction(const custom_type& type,
                                              std::vector<scalar_expr> expressions) {
  return custom_type_construction::create(type, std::move(expressions));
}

relative_order order_struct<compound_expr>::operator()(const compound_expr& a,
                                                       const compound_expr& b) const {
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
