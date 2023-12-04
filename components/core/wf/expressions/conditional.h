// Copyright 2023 Gareth Cross
#pragma once
#include "wf/constants.h"
#include "wf/expression.h"
#include "wf/expression_impl.h"

namespace wf {

class conditional {
 public:
  static constexpr std::string_view name_str = "Conditional";
  static constexpr bool is_leaf_node = false;

  conditional(Expr condition, Expr if_branch, Expr else_branch)
      : children_{std::move(condition), std::move(if_branch), std::move(else_branch)} {}

  bool is_identical_to(const conditional& other) const {
    return std::equal(children_.begin(), children_.end(), other.children_.begin(),
                      is_identical_struct<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(children_.begin(), children_.end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    Expr cond = operation(condition());
    if (cond.is_identical_to(constants::boolean_true)) {
      return operation(if_branch());
    } else if (cond.is_identical_to(constants::boolean_false)) {
      return operation(else_branch());
    }
    return create(std::move(cond), operation(if_branch()), operation(else_branch()));
  }

  // Create a new conditional.
  static Expr create(Expr condition, Expr if_branch, Expr else_branch);

  constexpr const Expr& condition() const noexcept { return children_[0]; }
  constexpr const Expr& if_branch() const noexcept { return children_[1]; }
  constexpr const Expr& else_branch() const noexcept { return children_[2]; }

  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

 protected:
  std::array<Expr, 3> children_;
};

template <>
struct hash_struct<conditional> {
  std::size_t operator()(const conditional& c) const {
    return hash_args(0, c.condition(), c.if_branch(), c.else_branch());
  }
};

}  // namespace wf
