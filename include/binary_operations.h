#pragma once
#include "expression_base.h"

namespace math {

class OperationBase {
 public:
  virtual int Precedence() const = 0;
  virtual ~OperationBase() = default;
};

template <typename Derived>
class OperationImpl : public OperationBase {
 public:
  int Precedence() const override { return Derived::OperatorPrecedence; }
  virtual ~OperationImpl() = default;
};

// Binary operation of two expressions.
template <typename Derived>
class BinaryOp : public ExpressionImpl<Derived>, public OperationImpl<Derived> {
 public:
  BinaryOp(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b) : a_(a), b_(b) {}

  // Move-constructor.
  BinaryOp(ExpressionBaseConstPtr&& a, ExpressionBaseConstPtr&& b)
      : a_(std::move(a)), b_(std::move(b)) {}

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool IsIdenticalToImplTyped(const BinaryOp<Derived>& other) const {
    return a_->IsIdenticalTo(other.a_) && b_->IsIdenticalTo(other.b_);
  }

  // Access left argument.
  const ExpressionBaseConstPtr& First() const { return a_; }

  // Access right argument.
  const ExpressionBaseConstPtr& Second() const { return b_; }

 protected:
  ExpressionBaseConstPtr a_;
  ExpressionBaseConstPtr b_;
};

// Addition operation.
class Addition : public BinaryOp<Addition> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;
  static constexpr int OperatorPrecedence = 1;
};

// Subtraction operation.
class Subtraction : public BinaryOp<Subtraction> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;
  static constexpr int OperatorPrecedence = 1;
};

// Multiplication operation.
class Multiplication : public BinaryOp<Multiplication> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;
  static constexpr int OperatorPrecedence = 2;
};

// Division operation.
class Division : public BinaryOp<Division> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;
  static constexpr int OperatorPrecedence = 3;
};

// Power operation.
class Power : public BinaryOp<Power> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;
  static constexpr int OperatorPrecedence = 4;
};

}  // namespace math
