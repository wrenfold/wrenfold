#pragma once
#include "expression_base.h"

namespace math {

template <typename Derived>
class UnaryOp : public ExpressionImpl<UnaryOp<Derived>> {
 public:
  explicit UnaryOp(const ExpressionBaseConstPtr& x) : x_(x) {}
  explicit UnaryOp(ExpressionBaseConstPtr&& x) : x_(std::move(x)) {}

 protected:
  ExpressionBaseConstPtr x_;
};

#if 0
class NaturalLog : public UnaryOp<NaturalLog> {
public:
  using UnaryOp::UnaryOp;

  ExpressionBaseConstPtr Diff(const Variable &var) const override {}

  std::string ToString() const { return fmt::format("ln({})", x_->ToString()); }

private:
};

class Power : public BinaryOp<Power> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable &var) const override { return {}; }

  std::string ToString() const { return FormatString("^"); }
};
#endif

}  // namespace math
