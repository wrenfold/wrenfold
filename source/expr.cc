#include "expr.h"

#include "assertions.h"
#include "constant_expressions.h"
#include "constants.h"
#include "formatting.h"
#include "variable.h"

namespace math {

static ExpressionBaseConstPtr MakeNumber(double x) {
  ASSERT(std::isfinite(x), "Number must be finite: x = {}", x);
  if (x == 0) {
    return Constants::Zero.GetImpl();
  } else if (x == 1) {
    return Constants::One.GetImpl();
  }
  return MakeExprBase<Number>(x);
}

Expr::Expr(const std::string& name) : impl_(new Variable(name)) {}

Expr::Expr(double x) : impl_(MakeNumber(x)) {}

std::string Expr::ToString() const {
  ASSERT(impl_);
  PlainFormatter formatter{};
  std::string result;
  impl_->Format(formatter, result);
  return result;
}

Expr Expr::Diff(const Expr& var, const int Reps) const {
  const Variable* const as_var = var.GetRaw<Variable>();
  ASSERT(as_var, "Arguments to diff() must be variables.");
  ASSERT_GREATER_OR_EQ(Reps, 0);

  DiffVisitor visitor{*as_var};
  ExpressionBaseConstPtr Result = impl_;
  for (int i = 0; i < Reps; ++i) {
    Result = Result->Diff(visitor);
  }
  return Expr{std::move(Result)};
}

}  // namespace math
