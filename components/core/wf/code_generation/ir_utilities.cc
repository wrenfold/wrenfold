// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_utilities.h"

#include <queue>

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/flat_hash_set.h>
WF_END_THIRD_PARTY_INCLUDES

#include "wf/code_generation/ir_value.h"
#include "wf/utility/algorithms.h"
#include "wf/utility/scoped_trace.h"

namespace wf {

void topological_sort_values(std::vector<ir::value_ptr>& operations) {
  WF_FUNCTION_TRACE();
  const std::size_t initial_size = operations.size();

  // We will de-queue values in increasing order of their names.
  struct reverse_order_values_by_name {
    bool operator()(const ir::const_value_ptr a, const ir::const_value_ptr b) const noexcept {
      return a->name() > b->name();
    }
  };

  // Queue every value that does not have an operand:
  std::priority_queue<ir::value_ptr, std::deque<ir::value_ptr>, reverse_order_values_by_name>
      queue{};
  for (const ir::value_ptr val : operations) {
    if (const bool no_inputs_in_block =
            none_of(val->operands(),
                    [&](const ir::const_value_ptr v) { return v->parent() == val->parent(); });
        no_inputs_in_block) {
      queue.push(val);
    }
  }

  absl::flat_hash_set<ir::const_value_ptr> visited{};
  visited.reserve(operations.size());

  operations.clear();
  while (!queue.empty()) {
    const ir::value_ptr top = queue.top();
    queue.pop();

    if (const auto [_, was_inserted] = visited.insert(top); !was_inserted) {
      continue;
    }
    operations.push_back(top);

    for (const ir::value_ptr c : top->consumers()) {
      if (c->parent() != top->parent()) {
        continue;
      }
      if (const bool all_operands_visited = all_of(
              c->operands(), [&](const ir::const_value_ptr v) { return visited.contains(v); });
          all_operands_visited) {
        queue.push(c);
      }
    }
  }

  WF_ASSERT_EQ(initial_size, operations.size(), "operations: [{}]", fmt::join(operations, ", "));
}

void remove_unused_values(std::vector<ir::value_ptr>& operations) {
  reverse_remove_if(operations, [](const ir::value_ptr v) {
    if (v->is_unused()) {
      v->remove();
      return true;
    }
    return false;
  });
}

void replace_redundant_copies(const absl::Span<const ir::value_ptr> operations) {
  for (ir::value_ptr v : operations) {
    // Is this a copy of another value already defined in the same block?
    if (v->is_op<ir::copy>() && v->first_operand()->parent() == v->parent()) {
      v->replace_with(v->first_operand());
    }
  }
}

}  // namespace wf
