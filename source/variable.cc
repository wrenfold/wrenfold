#include "variable.h"
#include "numeric_constants.h"

namespace math {

ExpressionBaseConstPtr Variable::Diff(const Variable& var) const {
  if (EqualsImplTyped(var)) {
    return NumericConstants::One;
  }
  return NumericConstants::Zero;
}

bool Variable::EqualsImplTyped(const Variable& other) const { return name_ == other.name_; }

}  // namespace math
