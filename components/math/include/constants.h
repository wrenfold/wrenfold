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
  static const Expr Infinity;
  static const Expr True;
  static const Expr False;
};

inline bool IsZero(const Expr& expr) { return expr.is_identical_to(Constants::Zero); }

inline bool IsOne(const Expr& expr) { return expr.is_identical_to(Constants::One); }

inline bool IsNegativeOne(const Expr& expr) { return expr.is_identical_to(Constants::NegativeOne); }

inline bool IsPi(const Expr& expr) { return expr.is_identical_to(Constants::Pi); }

inline bool IsInfinity(const Expr& expr) { return expr.is_identical_to(Constants::Infinity); }

}  // namespace math
