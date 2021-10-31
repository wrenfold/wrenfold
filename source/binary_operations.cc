#include "binary_operations.h"

namespace math {

ExpressionBaseConstPtr Addition::Diff(const Variable& var) const {
  return CreateAddition(a_->Diff(var), b_->Diff(var));
}

std::string Addition::ToString() const { return FormatString("+"); }

ExpressionBaseConstPtr Subtraction::Diff(const Variable& var) const {
  return CreateSubtraction(a_->Diff(var), b_->Diff(var));
}

std::string Subtraction::ToString() const { return FormatString("-"); }

ExpressionBaseConstPtr Multiplication::Diff(const Variable& var) const {
  return CreateAddition(CreateMultiplication(a_, b_->Diff(var)),
                        CreateMultiplication(a_->Diff(var), b_));
}

std::string Multiplication::ToString() const { return FormatString("*"); }

}  // namespace math
