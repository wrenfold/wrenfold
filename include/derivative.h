// Copyright 2022 Gareth Cross
#pragma once

#include "binary_operations.h"
#include "expression_fwd.h"
#include "functions.h"
#include "operation_utils.h"
#include "operations_fwd.h"
#include "variable.h"
#include "visitor.h"

namespace math {

class DiffVisitor final : public VisitorWithResultImpl<DiffVisitor> {
 public:
  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const Variable& argument) : argument_(argument) {}

  ExpressionBaseConstPtr Apply(const Addition& add) {
    return add.First()->Receive(*this) + add.Second()->Receive(*this);
  }

  ExpressionBaseConstPtr Apply(const Constant&) { return Constants::Zero.GetImpl(); }

  ExpressionBaseConstPtr Apply(const Division& div) {
    // Apply quotient rule:
    const auto& a = div.First();
    const auto& b = div.Second();
    const auto a_diff = div.First()->Receive(*this);
    const auto b_diff = div.Second()->Receive(*this);
    return (a_diff * b - b_diff * a) / (b * b);
  }

  ExpressionBaseConstPtr Apply(const Multiplication& div) {
    // Apply product rule:
    const auto& a = div.First();
    const auto& b = div.Second();
    return a * b->Receive(*this) + a->Receive(*this) * b;
  }

  ExpressionBaseConstPtr Apply(const Number&) { return Constants::Zero.GetImpl(); }

  ExpressionBaseConstPtr Apply(const Power& pow) {
    const auto& a = pow.First();
    const auto& b = pow.Second();
    const auto a_diff = pow.First()->Receive(*this);
    const auto b_diff = pow.Second()->Receive(*this);
    return b * (a ^ (b - Constants::One.GetImpl())) * a_diff + (a ^ b) * Log(a) * b_diff;
  }

  ExpressionBaseConstPtr Apply(const Subtraction& sub) {
    return sub.First()->Receive(*this) - sub.Second()->Receive(*this);
  }

  ExpressionBaseConstPtr Apply(const Variable& var) {
    if (var.IsIdenticalToImplTyped(argument_)) {
      return Constants::One.GetImpl();
    }
    return Constants::Zero.GetImpl();
  }

 private:
  const Variable& argument_;
};

}  // namespace math
