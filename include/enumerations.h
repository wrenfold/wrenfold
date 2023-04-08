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

// Describe the relative order of two expressions (a, b)
// This is for operations like sorting, not for expressing mathematical relations.
enum class RelativeOrder : int {
  // a < b
  LessThan = -1,
  // a == b
  Equal = 0,
  // a > b
  GreaterThan = 1,
};

// Index type for matrices/vectors.
using index_t = int;

}  // namespace math
