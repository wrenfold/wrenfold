#pragma once
#include "wf/boolean_expression.h"
#include "wf/expression.h"

namespace wf {
class constants {
 public:
  static const scalar_expr zero;
  static const scalar_expr one;
  static const scalar_expr pi;
  static const scalar_expr euler;
  static const scalar_expr negative_one;
  static const scalar_expr complex_infinity;
  static const scalar_expr undefined;

  static const boolean_expr boolean_true;
  static const boolean_expr boolean_false;
};

inline bool is_zero(const scalar_expr& expr) { return expr.is_identical_to(constants::zero); }

inline bool is_one(const scalar_expr& expr) { return expr.is_identical_to(constants::one); }

inline bool is_negative_one(const scalar_expr& expr) {
  return expr.is_identical_to(constants::negative_one);
}

inline bool is_pi(const scalar_expr& expr) { return expr.is_identical_to(constants::pi); }

inline bool is_complex_infinity(const scalar_expr& expr) {
  return expr.is_identical_to(constants::complex_infinity);
}

inline bool is_undefined(const scalar_expr& expr) {
  return expr.is_identical_to(constants::undefined);
}

inline bool is_numeric_or_constant(const scalar_expr& expr) {
  return expr.is_type<symbolic_constant, integer_constant, float_constant, rational_constant>();
}

}  // namespace wf
