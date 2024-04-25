// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/boolean_expression.h"
#include "wf/expression.h"

namespace wf {

// A relational expression for equalities and inequalities.
class relational {
 public:
  static constexpr std::string_view name_str = "Relational";
  static constexpr bool is_leaf_node = false;

  relational(const relational_operation operation, scalar_expr left, scalar_expr right) noexcept
      : operation_(operation), children_{std::move(left), std::move(right)} {}

  // Implement ExpressionImpl::Map
  template <typename Operation>
  boolean_expr map_children(Operation&& operation) const {
    return relational::create(operation_, operation(left()), operation(right()));
  }

  // Create a relational operation.
  static boolean_expr create(relational_operation operation, scalar_expr left, scalar_expr right);

  constexpr relational_operation operation() const noexcept { return operation_; }
  constexpr const scalar_expr& left() const noexcept { return children_[0]; }
  constexpr const scalar_expr& right() const noexcept { return children_[1]; }

  constexpr std::string_view operation_string() const noexcept {
    return string_from_relational_operation(operation_);
  }

  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

 protected:
  relational_operation operation_;
  std::array<scalar_expr, 2> children_;
};

template <>
struct hash_struct<relational> {
  std::size_t operator()(const relational& rel) const {
    return hash_args(static_cast<std::size_t>(rel.operation()), rel.left(), rel.right());
  }
};

template <>
struct is_identical_struct<relational> {
  std::size_t operator()(const relational& a, const relational& b) const {
    return a.operation() == b.operation() && are_identical(a.left(), b.left()) &&
           are_identical(a.right(), b.right());
  }
};

template <>
struct order_struct<relational> {
  relative_order operator()(const relational& a, const relational& b) const {
    if (a.operation() < b.operation()) {
      return relative_order::less_than;
    } else if (a.operation() > b.operation()) {
      return relative_order::greater_than;
    }
    return lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
