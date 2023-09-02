// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "expression_impl.h"

namespace math {

// A relational expression for equalities and inequalities.
class Relational : public ExpressionImpl<Relational> {
 public:
  static constexpr std::string_view NameStr = "Relational";
  static constexpr bool IsLeafNode = false;

  Relational(RelationalOperation operation, Expr left, Expr right)
      : operation_(operation), left_(std::move(left)), right_(std::move(right)) {}

  // Base and exponent must match.
  bool IsIdenticalToImplTyped(const Relational& other) const {
    return operation_ == other.operation_ && left_.IsIdenticalTo(other.left_) &&
           right_.IsIdenticalTo(other.right_);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation operation) const {
    operation(left_);
    operation(right_);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation operation) const {
    return Relational::Create(operation_, operation(left_), operation(right_));
  }

  // Create a relational operation.
  static Expr Create(RelationalOperation operation, Expr left, Expr right);

  RelationalOperation Operation() const { return operation_; }
  const Expr& Left() const { return left_; }
  const Expr& Right() const { return right_; }

  constexpr std::string_view OperationString() const {
    return StringFromRelationalOperation(operation_);
  }

 protected:
  RelationalOperation operation_;
  Expr left_;
  Expr right_;
};

}  // namespace math
