#pragma once
#include "expression.h"

namespace math {
class Constants {
 public:
  static const Expr Zero;
  static const Expr One;
  static const Expr Pi;
  static const Expr Euler;
};

inline bool IsZero(const Expr& expr) { return expr.IsIdenticalTo(Constants::Zero); }

inline bool IsOne(const Expr& expr) { return expr.IsIdenticalTo(Constants::One); }

// List of symbolic constants.
enum class SymbolicConstants : int {
  Pi = 0,
  Euler,
};

}  // namespace math
