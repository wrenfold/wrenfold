// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/compound_expression.h"

#include "wf/expression_visitor.h"
#include "wf/index_range.h"
#include "wf/plain_formatter.h"
#include "wf/tree_formatter.h"

namespace wf {

std::string compound_expr::to_string() const {
  plain_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

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

compound_expr create_custom_type_argument(const custom_type& type, const std::size_t arg_index) {
  return compound_expr{std::in_place_type_t<custom_type_argument>{}, type, arg_index};
}

compound_expr create_custom_type_construction(const custom_type& type,
                                              std::vector<scalar_expr> expressions) {
  return custom_type_construction::create(type, std::move(expressions));
}

}  // namespace wf
