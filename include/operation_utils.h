#pragma once
#include "expression_fwd.h"

namespace math {

// Create an addition expression.
ExpressionBaseConstPtr Add(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b);

// Create a subtraction expression.
ExpressionBaseConstPtr Sub(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b);

// Create a multiplication expression.
ExpressionBaseConstPtr Mul(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b);

// Create a division expression.
ExpressionBaseConstPtr Div(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b);

// Create a power expression.
ExpressionBaseConstPtr Pow(const ExpressionBaseConstPtr& a, const ExpressionBaseConstPtr& b);

// Create a negation expression.
ExpressionBaseConstPtr Negate(const ExpressionBaseConstPtr& x);

}  // namespace math
