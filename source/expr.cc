#include "expr.h"

#include "assertions.hpp"
#include "variable.h"

namespace math {

Expr Expr::Diff(const Expr& var, const int Reps) const {
  const Variable* const as_var = var.GetRaw<Variable>();
  ASSERT(as_var, "Arguments to diff() must be variables.");
  ASSERT_GREATER_OR_EQ(Reps, 0);

  ExpressionBaseConstPtr Result = impl_;
  for (int i = 0; i < Reps; ++i) {
    Result = Result->Diff(*as_var);
  }
  return Expr{std::move(Result)};
}

std::string Expr::ToString() const { return impl_->ToString(); }

}  // namespace math
