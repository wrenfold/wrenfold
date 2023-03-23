#include "expression.h"

#include "assertions.h"
#include "constants.h"
#include "derivative.h"
#include "distribute.h"
#include "expressions/numeric_expressions.h"
#include "expressions/variable.h"
#include "plain_formatter.h"
#include "substitute.h"

namespace math {

Expr::Expr(const std::string_view name) : Expr(MakeExpr<Variable>(std::string{name})) {}

Expr Expr::FromFloat(const double x) {
  if (x == 0) {
    return Constants::Zero;
  }
  return Float::Create(x);
}

Expr Expr::FromInt(const std::int64_t x) { return Integer::Create(x); }

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
  Expr result = *this;
  for (int i = 0; i < reps; ++i) {
    result = VisitStruct(result, visitor);
  }
  return result;
}

Expr Expr::Distribute() const { return VisitStruct(*this, DistributeVisitor{}); }

Expr Expr::Subs(const Expr& target, const Expr& replacement) const {
  return Substitute(*this, target, replacement);
}

// TODO: It would be good if these could be inlined for internal library code.
// In some cases, Expr can be a universal reference to avoid a shared_ptr copy.

Expr operator+(const Expr& a, const Expr& b) { return Addition::FromTwoOperands(a, b); }

Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::FromTwoOperands(Constants::NegativeOne, b);
}

Expr operator*(const Expr& a, const Expr& b) { return Multiplication::FromTwoOperands(a, b); }

Expr operator/(const Expr& a, const Expr& b) {
  if (TryCast<Matrix>(b)) {
    throw TypeError("Cannot divide by a matrix expression of type: {}", b.TypeName());
  }
  return Multiplication::FromTwoOperands(a, Power::Create(b, Constants::NegativeOne));
}

}  // namespace math
