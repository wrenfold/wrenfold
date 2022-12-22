#pragma once
#include <string>

// Fwd declarations relevant to formatting.
namespace math {

// Forward declare.
class ExpressionBase;
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

// Use PlainFormatter to format an expression.
std::string ToPlainString(const ExpressionBaseConstPtr& expr);

}  // namespace math
