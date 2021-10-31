#include "numeric_constants.h"

#include "constant.h"  //  for MakeNum

namespace math {

const ExpressionBaseConstPtr NumericConstants::Zero = MakeNum(0);
const ExpressionBaseConstPtr NumericConstants::One = MakeNum(1);

bool IsZero(const ExpressionBaseConstPtr& expr) { return expr->Equals(NumericConstants::Zero); }

bool IsOne(const ExpressionBaseConstPtr& expr) { return expr->Equals(NumericConstants::One); }

}  // namespace math
