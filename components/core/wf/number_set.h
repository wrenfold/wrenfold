// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/enumerations.h"

namespace wf {
class scalar_expr;

// Determine what set of numbers an expression belongs to.
// Returns `NumberSet::unknown` if the set cannot be determined.
number_set determine_numeric_set(const scalar_expr& x);

}  // namespace wf
