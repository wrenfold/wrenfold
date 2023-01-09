// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_base.h"

namespace math {

class Expr;
class ExpressionConcept;

enum class Precedence : int {
  Addition = 0,
  Multiplication,
  Power,
  None = std::numeric_limits<int>::max(),
};

}  // namespace math
