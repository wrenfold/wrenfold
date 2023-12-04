// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/expression_impl.h"

namespace math {

// A relational expression for equalities and inequalities.
class Relational {
 public:
  static constexpr std::string_view name_str = "Relational";
  static constexpr bool is_leaf_node = false;

  Relational(relational_operation operation, Expr left, Expr right)
      : operation_(operation), children_{std::move(left), std::move(right)} {}

  // Base and exponent must match.
  bool is_identical_to(const Relational& other) const {
    return operation_ == other.operation_ && left().is_identical_to(other.left()) &&
           right().is_identical_to(other.right());
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(children_.begin(), children_.end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return Relational::create(operation_, operation(left()), operation(right()));
  }

  // Create a relational operation.
  static Expr create(relational_operation operation, Expr left, Expr right);

  constexpr relational_operation operation() const noexcept { return operation_; }
  constexpr const Expr& left() const noexcept { return children_[0]; }
  constexpr const Expr& right() const noexcept { return children_[1]; }

  constexpr std::string_view operation_string() const noexcept {
    return string_from_relational_operation(operation_);
  }

  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

 protected:
  relational_operation operation_;
  std::array<Expr, 2> children_;
};

template <>
struct hash_struct<Relational> {
  std::size_t operator()(const Relational& rel) const {
    return hash_args(static_cast<std::size_t>(rel.operation()), rel.left(), rel.right());
  }
};

}  // namespace math
