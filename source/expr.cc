#include "expr.h"

#include "assertions.h"
#include "constant_expressions.h"
#include "constants.h"
#include "derivative.h"
#include "formatting.h"
#include "operation_utils.h"
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

Expr Expr::operator-() const { return Expr{Negate(impl_)}; }

Expr Expr::Diff(const Expr& var, const int reps) const {
  const Variable* const as_var = var.GetRaw<Variable>();
  ASSERT(as_var, "Arguments to diff() must be variables.");
  ASSERT_GREATER_OR_EQ(reps, 0);

  DiffVisitor visitor{*as_var};
  ExpressionBaseConstPtr Result = impl_;
  for (int i = 0; i < reps; ++i) {
    Result = Result->Receive(visitor);
  }
  return Expr{std::move(Result)};
}

Expr operator*(const Expr& a, const Expr& b) { return Expr{Mul(a.GetImpl(), b.GetImpl())}; }
Expr operator+(const Expr& a, const Expr& b) { return Expr{Add(a.GetImpl(), b.GetImpl())}; }
Expr operator-(const Expr& a, const Expr& b) { return Expr{Sub(a.GetImpl(), b.GetImpl())}; }
Expr operator/(const Expr& a, const Expr& b) { return Expr{Div(a.GetImpl(), b.GetImpl())}; }
Expr operator^(const Expr& a, const Expr& b) { return Expr{Pow(a.GetImpl(), b.GetImpl())}; }

}  // namespace math
