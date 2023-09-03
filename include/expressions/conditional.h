// Copyright 2023 Gareth Cross
#pragma once
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

  bool IsIdenticalTo(const Conditional& other) const {
    return condition_.IsIdenticalTo(other.condition_) &&
           if_branch_.IsIdenticalTo(other.if_branch_) &&
           else_branch_.IsIdenticalTo(other.else_branch_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation operation) const {
    operation(condition_);
    operation(if_branch_);
    operation(else_branch_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation operation) const {
    return Create(operation(condition_), operation(if_branch_), operation(else_branch_));
  }

  // Create a new conditional.
  static Expr Create(Expr condition, Expr if_branch, Expr else_branch);

  const Expr& Condition() const { return condition_; }
  const Expr& IfBranch() const { return if_branch_; }
  const Expr& ElseBranch() const { return else_branch_; }

 protected:
  Expr condition_;
  Expr if_branch_;
  Expr else_branch_;
};

template <>
struct Hash<Conditional> {
  std::size_t operator()(const Conditional& c) const {
    return HashArgs(0, c.Condition(), c.IfBranch(), c.ElseBranch());
  }
};

}  // namespace math
