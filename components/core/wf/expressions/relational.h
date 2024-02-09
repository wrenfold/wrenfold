// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"

namespace wf {

// A relational expression for equalities and inequalities.
class relational {
 public:
  static constexpr std::string_view name_str = "Relational";
  static constexpr bool is_leaf_node = false;

  relational(relational_operation operation, scalar_expr left, scalar_expr right)
      : operation_(operation), children_{std::move(left), std::move(right)} {}

  // Base and exponent must match.
  bool is_identical_to(const relational& other) const {
    return operation_ == other.operation_ && left().is_identical_to(other.left()) &&
           right().is_identical_to(other.right());
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return relational::create(operation_, operation(left()), operation(right()));
  }

  // Create a relational operation.
  static scalar_expr create(relational_operation operation, scalar_expr left, scalar_expr right);

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

}  // namespace wf
