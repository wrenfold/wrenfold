#include "constant_expressions.h"

#include "constants.h"

namespace math {

ExpressionBaseConstPtr Number::Diff(const Variable&) const { return Constants::Zero; }

ExpressionBaseConstPtr Constant::Diff(const Variable&) const { return Constants::Zero; }

}  // namespace math
