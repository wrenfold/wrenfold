#pragma once
#include "expression.h"

// TODO: These are all n-ary ops (except for power), and should changed.
namespace math {

// Binary operation of two expressions.
template <typename Derived>
class BinaryOp : public ExpressionImpl<Derived> {
 public:
  BinaryOp(const Expr& a, const Expr& b) : a_(a), b_(b) {}

  // Move-constructor.
  BinaryOp(Expr&& a, Expr&& b) : a_(std::move(a)), b_(std::move(b)) {}

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool IsIdenticalToImplTyped(const BinaryOp<Derived>& other) const {
    return a_.IsIdenticalTo(other.a_) && b_.IsIdenticalTo(other.b_);
  }

  // Access left argument.
  const Expr& First() const { return a_; }

  // Access right argument.
  const Expr& Second() const { return b_; }

  // Access name of the operation.
  constexpr const char* Name() const { return Derived::NameStr; }

 protected:
  Expr a_;
  Expr b_;
};

// Addition operation.
class Addition : public BinaryOp<Addition> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr int OperatorPrecedence = 1;
  static constexpr const char* NameStr = "Addition";
};

// Subtraction operation.
class Subtraction : public BinaryOp<Subtraction> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr int OperatorPrecedence = 1;
  static constexpr const char* NameStr = "Subtraction";
};

// Multiplication operation.
class Multiplication : public BinaryOp<Multiplication> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr int OperatorPrecedence = 2;
  static constexpr const char* NameStr = "Multiplication";
};

// Division operation.
class Division : public BinaryOp<Division> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr int OperatorPrecedence = 3;
  static constexpr const char* NameStr = "Division";
};

// Power operation.
class Power : public BinaryOp<Power> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr int OperatorPrecedence = 4;
  static constexpr const char* NameStr = "Power";
};

}  // namespace math
