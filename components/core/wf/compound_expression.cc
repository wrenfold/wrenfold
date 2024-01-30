// Copyright 2024 Gareth Cross
#include "wf/compound_expression.h"

#include "wf/index_range.h"
#include "wf/plain_formatter.h"
#include "wf/visit.h"

namespace wf {

std::string compound_expr::to_string() const {
  plain_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

std::vector<Expr> create_expression_elements(const compound_expr& provenance,
                                             const std::size_t num) {
  std::vector<Expr> elements{};
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
                                              std::vector<Expr> expressions) {
  return custom_type_construction::create(type, std::move(expressions));
}

}  // namespace wf
