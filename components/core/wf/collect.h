// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"

namespace wf {

// Collect powers of the specified terms.
// Terms are prioritized by their order in `terms`.
// Implemented in collect.cc
scalar_expr collect_many(const scalar_expr& arg, absl::Span<const scalar_expr> terms);

// Version of `collect_many` that accepts a single term.
scalar_expr collect(const scalar_expr& arg, const scalar_expr& term);

}  // namespace wf
