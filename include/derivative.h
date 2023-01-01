// Copyright 2022 Gareth Cross
#pragma once

#include "expression.h"
#include "expressions/addition.h"
#include "expressions/multiplication.h"
#include "expressions/variable.h"
#include "functions.h"
#include "operations_fwd.h"
#include "operations_inline.h"
#include "visitor_base.h"

namespace math {

// Visitor that takes the derivative of an input expression.
class DiffVisitor final : public VisitorImpl<DiffVisitor, Expr> {
 public:
  constexpr static VisitorPolicy Policy = VisitorPolicy::CompileError;

  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const Variable& argument) : argument_(argument) {}

  // Differentiate every argument to make a new sum.
  Expr Apply(const Addition& add) {
    Expr result = add[0].Receive(*this);
    for (std::size_t i = 1; i < add.Arity(); ++i) {
      result = result + add[i].Receive(*this);
    }
    return result;
  }

  Expr Apply(const Constant&) { return Constants::Zero; }

  // a * b
  // a' * b + a * b'
  // a * b * c
  // a' * b * c + a * b' * c + a * b * c'
  Expr Apply(const Multiplication& mul) {
    // Differentiate wrt every argument:
    // TODO: Try to make sure all this stuff gets moved instead of copied.
    Expr sum = Constants::Zero;
    for (std::size_t i = 0; i < mul.Arity(); ++i) {
      Expr term_i = Constants::One;
      for (std::size_t j = 0; j < mul.Arity(); ++j) {
        if (j == i) {
          term_i = term_i * mul[j].Receive(*this);
        } else {
          term_i = term_i * mul[j];
        }
      }
      sum = sum + term_i;
    }
    return sum;
  }

  Expr Apply(const NaturalLog& log) { return Power::Create(log.Inner(), Constants::NegativeOne); }

  Expr Apply(const Integer&) { return Constants::Zero; }

  Expr Apply(const Float&) { return Constants::Zero; }

  Expr Apply(const Power& pow) {
    const auto& a = pow.Base();
    const auto& b = pow.Exponent();
    const auto a_diff = a.Receive(*this);
    const auto b_diff = b.Receive(*this);
    return b * Power::Create(a, b - Constants::One) * a_diff +
           Power::Create(a, b) * NaturalLog::Create(a) * b_diff;
  }

  Expr Apply(const Variable& var) {
    if (var.IsIdenticalToImplTyped(argument_)) {
      return Constants::One;
    }
    return Constants::Zero;
  }

 private:
  Variable argument_;
};

}  // namespace math
