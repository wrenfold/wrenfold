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

inline bool is_zero(const Expr& expr) { return expr.is_identical_to(Constants::Zero); }

inline bool is_one(const Expr& expr) { return expr.is_identical_to(Constants::One); }

inline bool is_negative_one(const Expr& expr) {
  return expr.is_identical_to(Constants::NegativeOne);
}

inline bool is_pi(const Expr& expr) { return expr.is_identical_to(Constants::Pi); }

inline bool is_infinity(const Expr& expr) { return expr.is_identical_to(Constants::Infinity); }

}  // namespace math
