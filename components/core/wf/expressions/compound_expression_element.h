// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/compound_expression.h"

namespace wf {

// A single member of a compound expression. This expression acts similar to a variable - it is a
// single field from a custom type that was produced by a function invocation.
class compound_expression_element {
 public:
  static constexpr std::string_view name_str = "CompoundExpressionElement";
  static constexpr bool is_leaf_node = false;

  compound_expression_element(compound_expr provenance, const std::size_t index) noexcept
      : provenance_(std::move(provenance)), index_(index) {}

  // The invocation that produced this expression.
  constexpr const compound_expr& provenance() const noexcept { return provenance_; }

  // Index into the flattened struct denoting which member we are accessing.
  constexpr std::size_t index() const noexcept { return index_; }

  // Iterators for iterating over our single expression.
  constexpr auto begin() const noexcept { return &provenance_; }
  constexpr auto end() const noexcept { return begin() + 1; }

  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return create(operation(provenance_), index_);
  }

  constexpr absl::Span<const compound_expr> children() const noexcept {
    return {std::addressof(provenance_), static_cast<std::size_t>(1)};
  }

  // Create a reference to an element of a compound expression. This will simplify if the compound
  // expression is a constructor.
  static scalar_expr create(compound_expr provenance, std::size_t index);

 private:
  compound_expr provenance_;
  std::size_t index_;
};

template <>
struct hash_struct<compound_expression_element> {
  std::size_t operator()(const compound_expression_element& func) const noexcept {
    return hash_args(func.index(), func.provenance());
  }
};

template <>
struct is_identical_struct<compound_expression_element> {
  bool operator()(const compound_expression_element& a,
                  const compound_expression_element& b) const {
    return a.index() == b.index() && are_identical(a.provenance(), b.provenance());
  }
};

template <>
struct order_struct<compound_expression_element> {
  relative_order operator()(const compound_expression_element& a,
                            const compound_expression_element& b) const {
    return order_by(a.provenance(), b.provenance()).and_then_by(a.index(), b.index());
  }
};

}  // namespace wf
