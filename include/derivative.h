// Copyright 2022 Gareth Cross
#pragma once

#include "expression_fwd.h"
#include "operations_fwd.h"

namespace math {

class DiffVisitor {
 public:
  // Construct w/ const reference to the variable to differentiate wrt to.
  // Must remain in scope for the duration of evaluation.
  explicit DiffVisitor(const Variable& argument) : argument_(argument) {}

  ExpressionBaseConstPtr Diff(const Addition& add) const;
  ExpressionBaseConstPtr Diff(const Constant& constant) const;
  ExpressionBaseConstPtr Diff(const Division& div) const;
  ExpressionBaseConstPtr Diff(const Multiplication& mul) const;
  ExpressionBaseConstPtr Diff(const Number& num) const;
  ExpressionBaseConstPtr Diff(const Power& pow) const;
  ExpressionBaseConstPtr Diff(const Subtraction& sub) const;
  ExpressionBaseConstPtr Diff(const Negation& neg) const;
  ExpressionBaseConstPtr Diff(const NaturalLog& log) const;
  ExpressionBaseConstPtr Diff(const Variable& var) const;

 private:
  // Variable to differentiate with respect to.
  const Variable& argument_;
};

}  // namespace math
