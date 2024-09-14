// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/ir_value.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Which category of operations to binarize. We do each one in a separate step.
enum class binarize_type { add, mul };

// A pair of values used in a binary operation: `a + b` or `a * b`.
using value_pair = std::tuple<ir::value_ptr, ir::value_ptr>;

// Pair together a value (that is downstream of an add or mul) with the multiplicity
// of the operands.
struct value_multiplicity {
  // A value that consumes a given pair of operands.
  ir::value_ptr downstream_value;
  // # of times those operands appear in `downstream_value`.
  std::size_t multiplicity;

  constexpr value_multiplicity(const ir::value_ptr v, const std::size_t m) noexcept
      : downstream_value(v), multiplicity(m) {}
};

using vector_of_multiplicities = absl::InlinedVector<value_multiplicity, 4>;

struct value_pair_with_total_multiplicity {
  // A pair of operands (a, b)
  value_pair pair;
  // Consumers of the (a, b) pair and the # of times they consume this particular pair.
  vector_of_multiplicities consuming_values;
  // Sum of `consuming_values`.
  std::size_t total_multiplicity;

  value_pair_with_total_multiplicity(const value_pair& p, vector_of_multiplicities v,
                                     const std::size_t total) noexcept
      : pair(p), consuming_values(std::move(v)), total_multiplicity(total) {}
};

// Identify pairs of operands to multiplications or additions that should be extracted and re-used.
std::vector<value_pair_with_total_multiplicity> identify_operations_to_binarize(
    absl::Span<const ir::value_ptr> operations,
    const std::unordered_set<std::uint32_t>& active_values, binarize_type operation);

}  // namespace wf
