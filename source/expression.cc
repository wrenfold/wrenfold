#include "expression.h"

#include "assertions.h"
#include "constant_expressions.h"
#include "constants.h"
#include "derivative.h"
#include "formatting.h"
#include "variable.h"

namespace math {

static Expr ExprFromDouble(const double x) {
  if (x == 0) {
    return Constants::Zero;
  } else if (x == 1) {
    return Constants::One;
  } else if (std::floor(x) == x) {
    return MakeExpr<Integer>(x);
  }
  return MakeExpr<Float>(x);
}

Expr::Expr(const std::string& name) : impl_(new Variable(name)) {}

Expr::Expr(const double x) : Expr(ExprFromDouble(x)) {}

std::string Expr::ToString() const {
  ASSERT(impl_);
  PlainFormatter formatter{};
  VisitStruct(*this, formatter);
  return formatter.GetOutput();
}

Expr Expr::operator-() const { return Negation::Create(*this); }

Expr Expr::Diff(const Expr& var, const int reps) const {
  const Variable* const as_var = var.GetRaw<Variable>();
  ASSERT(as_var, "Arguments to diff() must be variables.");
  ASSERT_GREATER_OR_EQ(reps, 0);

  DiffVisitor visitor{*as_var};
  Expr Result = *this;
  for (int i = 0; i < reps; ++i) {
    Result = Result.Receive(visitor);
  }
  return Result;
}

}  // namespace math
