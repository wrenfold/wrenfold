#pragma once
#include "wf/expression.h"

namespace math {
class constants {
 public:
  static const Expr zero;
  static const Expr one;
  static const Expr pi;
  static const Expr euler;
  static const Expr negative_one;
  static const Expr complex_infinity;
  static const Expr boolean_true;
  static const Expr boolean_false;
  static const Expr undefined;
};

inline bool is_zero(const Expr& expr) { return expr.is_identical_to(constants::zero); }

inline bool is_one(const Expr& expr) { return expr.is_identical_to(constants::one); }

inline bool is_negative_one(const Expr& expr) {
  return expr.is_identical_to(constants::negative_one);
}

inline bool is_pi(const Expr& expr) { return expr.is_identical_to(constants::pi); }

inline bool is_complex_infinity(const Expr& expr) {
  return expr.is_identical_to(constants::complex_infinity);
}

inline bool is_undefined(const Expr& expr) { return expr.is_identical_to(constants::undefined); }

inline bool is_numeric_or_constant(const Expr& expr) {
  return expr.is_type<symbolic_constant, integer_constant, float_constant, rational_constant>();
}

}  // namespace math
