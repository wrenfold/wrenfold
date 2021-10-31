#include "unary_operations.h"

#include <fmt/format.h>

namespace math {

ExpressionBaseConstPtr Negate::Diff(const Variable& var) const {
  return CreateNegation(x_->Diff(var));
}

ExpressionBaseConstPtr NaturalLog::Diff(const Variable& var) const {
  return CreateDivision(x_->Diff(var), x_);
}

} // namespace math
