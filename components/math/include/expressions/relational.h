// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "expression_impl.h"

namespace math {

// A relational expression for equalities and inequalities.
class Relational {
 public:
  static constexpr std::string_view NameStr = "Relational";
  static constexpr bool IsLeafNode = false;

  Relational(RelationalOperation operation, Expr left, Expr right)
      : operation_(operation), left_(std::move(left)), right_(std::move(right)) {}

  // Base and exponent must match.
  bool is_identical_to(const Relational& other) const {
    return operation_ == other.operation_ && left_.is_identical_to(other.left_) &&
           right_.is_identical_to(other.right_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void iterate(Operation&& operation) const {
    operation(left_);
    operation(right_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return Relational::create(operation_, operation(left_), operation(right_));
  }

  // Create a relational operation.
  static Expr create(RelationalOperation operation, Expr left, Expr right);

  constexpr RelationalOperation operation() const { return operation_; }
  const Expr& left() const { return left_; }
  const Expr& right() const { return right_; }

  constexpr std::string_view operation_string() const {
    return string_from_relational_operation(operation_);
  }

 protected:
  RelationalOperation operation_;
  Expr left_;
  Expr right_;
};

template <>
struct Hash<Relational> {
  std::size_t operator()(const Relational& rel) const {
    return HashArgs(static_cast<std::size_t>(rel.operation()), rel.left(), rel.right());
  }
};

}  // namespace math
