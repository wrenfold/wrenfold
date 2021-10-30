#include "expressions.h"

#include <fmt/format.h>

namespace math {

const ExpressionBaseConstPtr NumericConstants::Zero = MakeNum(0);
const ExpressionBaseConstPtr NumericConstants::One = MakeNum(1);

Expr Expr::Diff(const Expr &var, const int Reps) const {
  const Variable *const as_var = var.GetRaw<Variable>();
  if (!as_var) {
    throw std::runtime_error(
        "Expression for taking derivative must be a variable.");
  }

  ExpressionBaseConstPtr Result = impl_;
  for (int i = 0; i < Reps; ++i) {
    Result = Result->Diff(*as_var);
  }
  return {std::move(Result)};
}

std::string Expr::ToString() const { return impl_->ToString(); }

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

bool IsZero(const ExpressionBaseConstPtr &expr) {
  return expr->Equals(*NumericConstants::Zero);
}

bool IsOne(const ExpressionBaseConstPtr &expr) {
  return expr->Equals(*NumericConstants::One);
}

} // namespace math
