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

bool IsZero(const ExpressionBaseConstPtr& expr);
bool IsOne(const ExpressionBaseConstPtr& expr);

// List of symbolic constants.
enum class SymbolicConstants : int {
  Pi = 0,
  Euler,
};

}  // namespace math
