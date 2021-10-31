#pragma once
#include "expr.h"

namespace math {
class NumericConstants {
 public:
  static const ExpressionBaseConstPtr Zero;
  static const ExpressionBaseConstPtr One;
};

bool IsZero(const ExpressionBaseConstPtr& expr);
bool IsOne(const ExpressionBaseConstPtr& expr);

}  // namespace math
