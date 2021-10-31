#include "binary_operations.h"
#include "functions.h"

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

ExpressionBaseConstPtr Division::Diff(const Variable& var) const {
  Expr a{a_};  //  A bit wasteful since we are incrementing shared ptr here.
  Expr b{b_};
  Expr a_diff{a_->Diff(var)};
  Expr b_diff{b_->Diff(var)};
  return (a_diff * b - b_diff * a) / (b * b);
}

ExpressionBaseConstPtr Power::Diff(const Variable& var) const {
  Expr a{a_};
  Expr b{b_};
  Expr a_diff{a_->Diff(var)};
  Expr b_diff{b_->Diff(var)};
  return b * (a ^ (b - 1)) * a_diff + (a ^ b) * log(a) * b_diff;
}

}  // namespace math
