// Copyright 2024 Gareth Cross
#include "wf/boolean_expression.h"

#include "wf/constants.h"
#include "wf/plain_formatter.h"
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

boolean_expr boolean_expr::construct_implicit(const bool value) {
  // Return an existing constant instead of allocating a new one:
  if (value) {
    return constants::boolean_true;
  } else {
    return constants::boolean_false;
  }
}

}  // namespace wf
