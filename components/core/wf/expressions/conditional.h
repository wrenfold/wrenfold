// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/constants.h"
#include "wf/expression.h"

namespace wf {

class conditional {
 public:
  static constexpr std::string_view name_str = "Conditional";
  static constexpr bool is_leaf_node = false;

  conditional(boolean_expr condition, scalar_expr if_branch, scalar_expr else_branch)
      : children_{std::move(condition), std::move(if_branch), std::move(else_branch)} {}

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    boolean_expr cond = operation(condition());
    if (cond.is_identical_to(constants::boolean_true)) {
      return operation(if_branch());
    } else if (cond.is_identical_to(constants::boolean_false)) {
      return operation(else_branch());
    }
    return create(std::move(cond), operation(if_branch()), operation(else_branch()));
  }

  // Create a new conditional.
  static scalar_expr create(boolean_expr condition, scalar_expr if_branch, scalar_expr else_branch);

  constexpr const boolean_expr& condition() const noexcept { return std::get<0>(children_); }
  constexpr const scalar_expr& if_branch() const noexcept { return std::get<1>(children_); }
  constexpr const scalar_expr& else_branch() const noexcept { return std::get<2>(children_); }

 protected:
  std::tuple<boolean_expr, scalar_expr, scalar_expr> children_;
};

template <>
struct hash_struct<conditional> {
  std::size_t operator()(const conditional& c) const noexcept {
    return hash_args(0, c.condition(), c.if_branch(), c.else_branch());
  }
};

template <>
struct is_identical_struct<conditional> {
  bool operator()(const conditional& a, const conditional& b) const {
    return are_identical(a.condition(), b.condition()) &&
           are_identical(a.if_branch(), b.if_branch()) &&
           are_identical(a.else_branch(), b.else_branch());
  }
};

template <>
struct order_struct<conditional> {
  relative_order operator()(const conditional& a, const conditional& b) const {
    return wf::order_by(a.condition(), b.condition())
        .and_then_by(a.if_branch(), b.if_branch())
        .and_then_by(a.else_branch(), b.else_branch());
  }
};

}  // namespace wf
