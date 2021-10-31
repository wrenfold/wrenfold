#pragma once
#include "expression_base.h"

namespace math {

template <typename Derived>
class UnaryOp : public ExpressionImpl<UnaryOp<Derived>> {
 public:
  explicit UnaryOp(const ExpressionBaseConstPtr& x) : x_(x) {}
  explicit UnaryOp(ExpressionBaseConstPtr&& x) : x_(std::move(x)) {}

  // Test unary ops for equality.
  bool IsIdenticalToImplTyped(const UnaryOp<Derived>& neg) const { return x_->IsIdenticalTo(neg.x_); }

  // Get inner expression.
  const ExpressionBaseConstPtr& Inner() const { return x_; }

 protected:
  ExpressionBaseConstPtr x_;
};

// Negate an expression.
class Negate : public UnaryOp<Negate> {
 public:
  using UnaryOp::UnaryOp;

  // Returns the derivative of the inner object, negated.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  std::string Format() const override;
};

class NaturalLog : public UnaryOp<NaturalLog> {
 public:
  using UnaryOp::UnaryOp;

  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  std::string Format() const override;
};

#if 0
class Power : public BinaryOp<Power> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable &var) const override { return {}; }

  std::string ToString() const { return FormatString("^"); }
};
#endif

}  // namespace math
