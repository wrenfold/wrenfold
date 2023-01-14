#include "expression.h"

#include "assertions.h"
#include "constants.h"
#include "derivative.h"
#include "distribute.h"
#include "expressions/numeric_expressions.h"
#include "expressions/variable.h"
#include "plain_formatter.h"

namespace math {

static Expr ExprFromDouble(const double x) {
  if (x == 0) {
    return Constants::Zero;
  }
  return MakeExpr<Float>(x);
}

Expr::Expr(const std::string& name) : Expr(MakeExpr<Variable>(name)) {}

Expr::Expr(const double x) : Expr(ExprFromDouble(x)) {}

Expr::Expr(const int64_t x) : Expr(Integer::Create(x)) {}

std::string Expr::ToString() const {
  ASSERT(impl_);
  PlainFormatter formatter{};
  VisitStruct(*this, formatter);
  return formatter.GetOutput();
}

Expr Expr::operator-() const {
  return Multiplication::FromTwoOperands(Constants::NegativeOne, *this);
}

Expr Expr::Diff(const Expr& var, const int reps) const {
  const Variable* const as_var = TryCast<Variable>(var);
  ASSERT(as_var, "Arguments to diff() must be variables.");
  ASSERT_GREATER_OR_EQ(reps, 0);

  DiffVisitor visitor{*as_var};
  Expr Result = *this;
  for (int i = 0; i < reps; ++i) {
    Result = Result.Receive(visitor);
  }
  return Result;
}

Expr Expr::Distribute() const { return VisitStruct(*this, DistributeVisitor{*this}).value(); }

// TODO: It would good if these could be inlined for internal library code.
// In some cases, Expr can be a universal reference to avoid a shared_ptr copy.

Expr operator*(const Expr& a, const Expr& b) { return Multiplication::FromTwoOperands(a, b); }

Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::FromTwoOperands(Constants::NegativeOne, b);
}

Expr operator/(const Expr& a, const Expr& b) {
  return Multiplication::FromTwoOperands(a, Power::Create(b, Constants::NegativeOne));
}

}  // namespace math
