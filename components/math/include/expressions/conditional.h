// Copyright 2023 Gareth Cross
#pragma once
#include "constants.h"
#include "expression.h"
#include "expression_impl.h"

namespace math {

class Conditional {
 public:
  static constexpr std::string_view NameStr = "Conditional";
  static constexpr bool IsLeafNode = false;

  Conditional(Expr condition, Expr if_branch, Expr else_branch)
      : condition_(std::move(condition)),
        if_branch_(std::move(if_branch)),
        else_branch_(std::move(else_branch)) {}

  bool is_identical_to(const Conditional& other) const {
    return condition_.is_identical_to(other.condition_) &&
           if_branch_.is_identical_to(other.if_branch_) &&
           else_branch_.is_identical_to(other.else_branch_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void iterate(Operation&& operation) const {
    operation(condition_);
    operation(if_branch_);
    operation(else_branch_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    Expr cond = operation(condition_);
    if (cond.is_identical_to(Constants::True)) {
      return operation(if_branch_);
    } else if (cond.is_identical_to(Constants::False)) {
      return operation(else_branch_);
    }
    return create(std::move(cond), operation(if_branch_), operation(else_branch_));
  }

  // Create a new conditional.
  static Expr create(Expr condition, Expr if_branch, Expr else_branch);

  const Expr& condition() const { return condition_; }
  const Expr& if_branch() const { return if_branch_; }
  const Expr& else_branch() const { return else_branch_; }

 protected:
  Expr condition_;
  Expr if_branch_;
  Expr else_branch_;
};

template <>
struct hash_struct<Conditional> {
  std::size_t operator()(const Conditional& c) const {
    return hash_args(0, c.condition(), c.if_branch(), c.else_branch());
  }
};

}  // namespace math
