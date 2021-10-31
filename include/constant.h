#pragma once
#include "expression_base.h"

namespace math {

/**
 * A scalar numerical constant like 0.23241, or 42.
 * TODO(gareth): Support integer types? For now double works fine.
 */
class Constant : public ExpressionImpl<Constant> {
 public:
  using ConstantType = double;

  // Construct from number.
  explicit Constant(ConstantType val) : val_(val) {}

  // Differentiating a constant produces zero.
  ExpressionBaseConstPtr Diff(const Variable&) const override;

  // Print to string
  std::string ToString() const override;

  // Check if numerical constants are completely identical.
  bool EqualsImplTyped(const Constant& other) const { return val_ == other.val_; }

 private:
  ConstantType val_;
};

// Make a numeric constant.
inline Expr MakeNum(Constant::ConstantType x) {
  return Expr{MakeExprBase<Constant>(x)};
}

}  // namespace math
