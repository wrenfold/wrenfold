// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

namespace math {

// Hash expression.
std::size_t Hash(const Expr& x);

// Object for use in maps.
struct HashObject {
  std::size_t operator()(const Expr& x) const { return Hash(x); }
};

}  // namespace math
