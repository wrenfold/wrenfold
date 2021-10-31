#include "expr.h"

#include "assertions.hpp"
#include "constant_expressions.h"
#include "constants.h"
#include "variable.h"

namespace math {

static ExpressionBaseConstPtr MakeNumber(double x) {
  if (x == 0) {
    return Constants::Zero;
  } else if (x == 1) {
    return Constants::One;
  }
  return MakeExprBase<Number>(x);
}

Expr::Expr(const std::string& name) : impl_(new Variable(name)) {}

Expr::Expr(double x) : impl_(MakeNumber(x)) {}

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

bool Expr::IsIdenticalTo(const Expr& other) const { return impl_->IsIdenticalTo(other); }

std::string Expr::ToString() const { return impl_->Format(); }

}  // namespace math
