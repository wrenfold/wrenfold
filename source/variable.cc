#include "variable.h"

#include "constants.h"

namespace math {

ExpressionBaseConstPtr Variable::Diff(const Variable& var) const {
  if (IsIdenticalToImplTyped(var)) {
    return Constants::One;
  }
  return Constants::Zero;
}

bool Variable::IsIdenticalToImplTyped(const Variable& other) const { return name_ == other.name_; }

}  // namespace math
