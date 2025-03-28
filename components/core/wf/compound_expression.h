// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"
#include "wf/expression_variant.h"
#include "wf/utility/ordering.h"

namespace wf {

struct compound_meta_type {};
template <>
struct type_list_trait<compound_meta_type> {
  // clang-format off
  using types = type_list<
    class external_function_invocation,
    class custom_type_argument,
    class custom_type_construction
  >;
  // clang-format on
};

// A compound expression is an expression whose value is an aggregate type, like a user-provided
// custom struct.
class compound_expr final : public expression_base<compound_expr, compound_meta_type> {
 public:
  using expression_base::expression_base;

  // Convert to human-readable string.
  std::string to_string() const;

  // Convert to expression tree string.
  std::string to_expression_tree_string() const;
};

template <>
struct order_struct<compound_expr> {
  relative_order operator()(const compound_expr& a, const compound_expr& b) const;
};

// Create `num` instances of `compound_expression_element` whose provenance is the provided compound
// expression.
std::vector<scalar_expr> create_expression_elements(const compound_expr& provenance,
                                                    std::size_t num);

// Create compound expression with type `custom_type_argument` using the provided type and argument.
compound_expr create_custom_type_argument(const class custom_type& type, std::string_view name);

// Create compound expression with type `custom_type_construction`.
compound_expr create_custom_type_construction(const custom_type& type,
                                              std::vector<scalar_expr> expressions);

}  // namespace wf
