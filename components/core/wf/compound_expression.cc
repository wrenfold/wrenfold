// Copyright 2024 Gareth Cross
#include "wf/compound_expression.h"

#include "wf/expressions/custom_function_invocation.h"
#include "wf/index_range.h"

namespace wf {

std::vector<Expr> create_expression_elements(const compound_expr& provenance,
                                             const std::size_t num) {
  std::vector<Expr> elements{};
  elements.reserve(num);
  for (const std::size_t index : make_range(num)) {
    elements.emplace_back(std::in_place_type_t<compound_expression_element>{}, provenance, index);
  }
  return elements;
}

compound_expr create_custom_type_argument(const custom_type& type, const std::size_t arg_index) {
  return compound_expr{std::in_place_type_t<custom_type_argument>{}, type, arg_index};
}

}  // namespace wf
