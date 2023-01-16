#pragma once
#include "expression.h"

namespace math {
class Constants {
 public:
  static const Expr Zero;
  static const Expr One;
  static const Expr Pi;
  static const Expr Euler;
  static const Expr NegativeOne;
};

inline bool IsZero(const Expr& expr) { return expr.IsIdenticalTo(Constants::Zero); }

inline bool IsOne(const Expr& expr) { return expr.IsIdenticalTo(Constants::One); }

inline bool IsNegativeOne(const Expr& expr) { return expr.IsIdenticalTo(Constants::NegativeOne); }

inline bool IsPi(const Expr& expr) { return expr.IsIdenticalTo(Constants::Pi); }

// List of symbolic constants.
enum class SymbolicConstants : int {
  Euler,
  Pi,
};

}  // namespace math
