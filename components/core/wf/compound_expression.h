// Copyright 2024 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/expression_variant.h"
#include "wf/ordering.h"

namespace wf {

struct compound_meta_type {};
template <>
struct type_list_trait<compound_meta_type> {
  // clang-format off
  using types = type_list<
    class custom_function_invocation,
    class custom_type_argument,
    class custom_type_construction
  >;
  // clang-format on
};

// An compound type expression is an expression whose value is an aggregate type,
// like a user-provided custom struct.
class compound_expr final : public expression_base<compound_expr, compound_meta_type> {
 public:
  using expression_base::expression_base;
};

template <>
struct order_struct<compound_expr> {
  // Implemented in ordering.cc
  relative_order operator()(const compound_expr& a, const compound_expr& b) const;
};

// Create `num` instances of `compound_expression_element` whose provenance is the provided compound
// expression.
std::vector<Expr> create_expression_elements(const compound_expr& provenance, std::size_t num);

// Create compound expression with type `custom_type_argument` using the provided type and argument.
compound_expr create_custom_type_argument(const class custom_type& type, std::size_t arg_index);

}  // namespace wf