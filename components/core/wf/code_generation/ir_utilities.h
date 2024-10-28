// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <vector>

#include "wf/code_generation/ir_value_fwd.h"
#include "wf/utility/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Topologically sort (by argument dependency) the values in `operations`.
// Vector is modified in-place.
void topological_sort_values(std::vector<ir::value_ptr>& operations);

// Remove any values that satisfy `ir::value::is_unused`.
// Traversal occurs in reverse order so that a chain of unused values can be stripped.
void remove_unused_values(std::vector<ir::value_ptr>& operations);

// Replace redundant copies by replacing the copied value with its operand.
// Only copies of values within the same block are replaced.
void replace_redundant_copies(absl::Span<const ir::value_ptr> operations);

}  // namespace wf
