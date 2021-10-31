#include "binary_operation_utils.h"

#include "binary_operations.h"
#include "constant.h"
#include "numeric_constants.h"

namespace math {

ExpressionBaseConstPtr CreateMultiplication(const ExpressionBaseConstPtr &a,
                                            const ExpressionBaseConstPtr &b) {
  if (IsZero(a) || IsZero(b)) {
    return NumericConstants::Zero;
  }
  if (IsOne(a)) {
    return b;
  }
  if (IsOne(b)) {
    return a;
  }
  return MakeExprBase<Multiplication>(a, b);
}

ExpressionBaseConstPtr CreateAddition(const ExpressionBaseConstPtr &a,
                                      const ExpressionBaseConstPtr &b) {
  // Simplify the case where one or more operands is zero
  const bool az = IsZero(a);
  const bool bz = IsZero(b);
  if (az && bz) {
    return NumericConstants::Zero;
  } else if (az) {
    return b;
  } else if (bz) {
    return a;
  }
  // Simplify the case where the operands are the same:
  if (a->Equals(b)) {
    return MakeExprBase<Multiplication>(MakeNum(2), a);
  }
  return MakeExprBase<Addition>(a, b);
}

}  // namespace math
