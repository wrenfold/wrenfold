// Copyright 2023 Gareth Cross
#pragma once
#include <limits>

namespace math {

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
