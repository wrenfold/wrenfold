#pragma once
#include <fmt/format.h>

#include "expression_base.h"

namespace math {

// Binary operation of two expressions.
template <typename Derived>
class BinaryOp : public ExpressionImpl<Derived> {
 public:
  BinaryOp(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b) : a_(a), b_(b) {}

  // Move-constructor.
  BinaryOp(ExpressionBaseConstPtr&& a, ExpressionBaseConstPtr&& b)
      : a_(std::move(a)), b_(std::move(b)) {}

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool EqualsImplTyped(const BinaryOp<Derived>& other) const {
    if (a_->Equals(other.a_) && b_->Equals(other.b_)) {
      return true;
    }
    if (Derived::IsCommutative) {
      if (a_->Equals(other.b_) && b_->Equals(other.a_)) {
        return true;
      }
    }
    return false;
  }

 protected:
  std::string FormatString(const std::string& op) const {
    // TODO: figure out when the braces are required, and when not:
    return fmt::format("({} {} {})", a_->ToString(), op, b_->ToString());
  }

 protected:
  ExpressionBaseConstPtr a_;
  ExpressionBaseConstPtr b_;
};

// Addition operation.
class Addition : public BinaryOp<Addition> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  std::string ToString() const override;
};

// Subtraction operation.
class Subtraction : public BinaryOp<Addition> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  std::string ToString() const override;
};

// Multiplication operation.
class Multiplication : public BinaryOp<Multiplication> {
 public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;
  std::string ToString() const override;
};

#if 0
class Division : public BinaryOp<Division> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable& var) const override {
    return CreateMultiplication(  );
  }

  std::string ToString() const { return FormatString("÷"); }

private:
};
#endif

}  // namespace math
