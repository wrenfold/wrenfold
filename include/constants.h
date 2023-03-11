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
};

inline bool IsZero(const Expr& expr) { return expr.IsIdenticalTo(Constants::Zero); }

inline bool IsOne(const Expr& expr) { return expr.IsIdenticalTo(Constants::One); }

inline bool IsNegativeOne(const Expr& expr) { return expr.IsIdenticalTo(Constants::NegativeOne); }

inline bool IsPi(const Expr& expr) { return expr.IsIdenticalTo(Constants::Pi); }

inline bool IsInfinity(const Expr& expr) { return expr.IsIdenticalTo(Constants::Infinity); }

// List of symbolic constants.
enum class SymbolicConstants : int {
  Euler,
  Infinity,
  Pi,
};

// A placeholder used to default initialize certain expressions during code-generation.
Expr GetUnfilledExprPlaceholder();

}  // namespace math
