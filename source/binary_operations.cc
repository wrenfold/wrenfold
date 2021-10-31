#include "binary_operations.h"

namespace math {

ExpressionBaseConstPtr Addition::Diff(const Variable& var) const {
  return CreateAddition(a_->Diff(var), b_->Diff(var));
}

ExpressionBaseConstPtr Subtraction::Diff(const Variable& var) const {
  return CreateSubtraction(a_->Diff(var), b_->Diff(var));
}

ExpressionBaseConstPtr Multiplication::Diff(const Variable& var) const {
  return CreateAddition(CreateMultiplication(a_, b_->Diff(var)),
                        CreateMultiplication(a_->Diff(var), b_));
}

ExpressionBaseConstPtr Division::Diff(const Variable&) const { return {}; }

}  // namespace math
