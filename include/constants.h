#pragma once
#include "expr.h"

namespace math {
class Constants {
 public:
  static const Expr Zero;
  static const Expr One;
  static const Expr Pi;
  static const Expr Euler;
};

inline bool IsZero(const ExpressionBaseConstPtr& expr) {
  return expr->IsIdenticalTo(Constants::Zero.GetImpl());
}

inline bool IsOne(const ExpressionBaseConstPtr& expr) {
  return expr->IsIdenticalTo(Constants::One.GetImpl());
}

// List of symbolic constants.
enum class SymbolicConstants : int {
  Pi = 0,
  Euler,
};

}  // namespace math
