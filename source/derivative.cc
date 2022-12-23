// Copyright 2022 Gareth Cross
#include "derivative.h"

#include "binary_operations.h"
#include "constants.h"
#include "functions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

ExpressionBaseConstPtr DiffVisitor::Diff(const Addition& add) const {
  return Add(add.First()->Diff(*this), add.Second()->Diff(*this));
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Constant&) const {
  return Constants::Zero.GetImpl();
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Division& div) const {
  const Expr a{div.First()};  //  A bit wasteful since we are incrementing shared ptr here.
  const Expr b{div.Second()};
  const Expr a_diff{div.First()->Diff(*this)};
  const Expr b_diff{div.Second()->Diff(*this)};
  return ((a_diff * b - b_diff * a) / (b * b)).GetImpl();
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Multiplication& mul) const {
  // Product rule:
  return Add(Mul(mul.First(), mul.Second()->Diff(*this)),
             Mul(mul.First()->Diff(*this), mul.Second()));
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Number&) const { return Constants::Zero.GetImpl(); }

ExpressionBaseConstPtr DiffVisitor::Diff(const Power& pow) const {
  const Expr a{pow.First()};
  const Expr b{pow.Second()};
  const Expr a_diff{pow.First()->Diff(*this)};
  const Expr b_diff{pow.Second()->Diff(*this)};
  return (b * (a ^ (b - 1)) * a_diff + (a ^ b) * math::log(a) * b_diff).GetImpl();
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Subtraction& sub) const {
  return Sub(sub.First()->Diff(*this), sub.Second()->Diff(*this));
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Negation& neg) const {
  return Negate(neg.Inner()->Diff(*this));
}

ExpressionBaseConstPtr DiffVisitor::Diff(const NaturalLog& log) const {
  return Div(log.Inner()->Diff(*this), log.Inner());
}

ExpressionBaseConstPtr DiffVisitor::Diff(const Variable& var) const {
  if (var.IsIdenticalToImplTyped(argument_)) {
    return Constants::One.GetImpl();
  }
  return Constants::Zero.GetImpl();
}

}  // namespace math
