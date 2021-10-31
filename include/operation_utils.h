#pragma once
#include <memory>

namespace math {

// Fwd declare.
class ExpressionBase;
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

// Create an addition expression.
ExpressionBaseConstPtr CreateAddition(const ExpressionBaseConstPtr& a,
                                      const ExpressionBaseConstPtr& b);

// Create a subtraction expression.
ExpressionBaseConstPtr CreateSubtraction(const ExpressionBaseConstPtr& a,
                                         const ExpressionBaseConstPtr& b);

// Create a multiplication expression.
ExpressionBaseConstPtr CreateMultiplication(const ExpressionBaseConstPtr& a,
                                            const ExpressionBaseConstPtr& b);

// Create a division expression.
ExpressionBaseConstPtr CreateDivision(const ExpressionBaseConstPtr& a,
                                      const ExpressionBaseConstPtr& b);

// Create a negation expression.
ExpressionBaseConstPtr CreateNegation(const ExpressionBaseConstPtr& x);

}  // namespace math
