// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_base.h"

namespace math {

class Expr;
class ExpressionConcept;

// Mathematical precedence of operators.
enum class Precedence : int {
  Addition = 0,
  Multiplication,
  Power,
  None = std::numeric_limits<int>::max(),
};

// Index type for matrices/vectors.
using index_t = int;

}  // namespace math
