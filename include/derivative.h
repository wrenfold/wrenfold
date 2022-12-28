// Copyright 2022 Gareth Cross
#pragma once

#include "binary_operations.h"
#include "expression.h"
#include "functions.h"
#include "operations_fwd.h"
#include "operations_inline.h"
#include "variable.h"
#include "visitor_base.h"

namespace math {

// Visitor that takes the derivative of an input expression.
class DiffVisitor final : public VisitorWithResultImpl<DiffVisitor> {
 public:
  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const Variable& argument) : argument_(argument) {}

  Expr Apply(const Addition& add) {
    return add.First().Receive(*this) + add.Second().Receive(*this);
  }

  Expr Apply(const Constant&) { return Constants::Zero; }

  Expr Apply(const Division& div) {
    // Apply quotient rule:
    const auto& a = div.First();
    const auto& b = div.Second();
    const auto a_diff = a.Receive(*this);
    const auto b_diff = b.Receive(*this);
    return (a_diff * b - b_diff * a) / (b * b);
  }

  Expr Apply(const Multiplication& div) {
    // Apply product rule:
    const auto& a = div.First();
    const auto& b = div.Second();
    return a * b.Receive(*this) + a.Receive(*this) * b;
  }

  Expr Apply(const Negation& neg) { return Negate(neg.Inner().Receive(*this)); }

  Expr Apply(const Number&) { return Constants::Zero; }

  Expr Apply(const Power& pow) {
    const auto& a = pow.First();
    const auto& b = pow.Second();
    const auto a_diff = a.Receive(*this);
    const auto b_diff = b.Receive(*this);
    return b * Pow(a, b - Constants::One) * a_diff + Pow(a, b) * Log(a) * b_diff;
  }

  Expr Apply(const Subtraction& sub) {
    return sub.First().Receive(*this) - sub.Second().Receive(*this);
  }

  Expr Apply(const Variable& var) {
    if (var.IsIdenticalToImplTyped(argument_)) {
      return Constants::One;
    }
    return Constants::Zero;
  }

 private:
  const Variable& argument_;
};

}  // namespace math
