// Copyright 2022 Gareth Cross
#pragma once
#include <string>

#include "expression_fwd.h"

namespace math {

// Print debug tree of expressions to a string.
std::string FormatDebugTree(const Expr& expr);

}  // namespace math
