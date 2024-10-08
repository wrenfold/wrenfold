// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/ir_block.h"

#include <deque>

namespace wf::ir {

void block::replace_descendant(ir::block_ptr target, ir::block_ptr replacement) {
  WF_ASSERT_NE(target, replacement);

  if (!operations_.empty()) {
    if (const ir::value_ptr jump_val = operations_.back(); jump_val->is_op<ir::jump_condition>()) {
      WF_ASSERT_EQ(2, descendants_.size());
    } else {
      WF_ASSERT_GE(1, descendants_.size());
    }
  }

  const ir::block_ptr self{this};
  target->remove_ancestor(self);
  replacement->add_ancestor(self);

  const auto it = std::find(descendants_.begin(), descendants_.end(), target);
  WF_ASSERT(it != descendants_.end());
  *it = replacement;
}

void block::add_ancestor(const block_ptr b) {
  WF_ASSERT(std::find(ancestors_.begin(), ancestors_.end(), b) == ancestors_.end(),
            "Attempted to insert duplicate into ancestor list: {}", b->name());
  ancestors_.push_back(b);
}

void block::remove_ancestor(const block_ptr b) {
  const auto it = std::find(ancestors_.begin(), ancestors_.end(), b);
  WF_ASSERT(it != ancestors_.end(), "Block {} is not an ancestor of {}", b->name(), name());
  ancestors_.erase(it);
}

void block::add_descendant(const block_ptr b) {
  WF_ASSERT(std::find(descendants_.begin(), descendants_.end(), b) == descendants_.end(),
            "Block {} already exists in descendants list: {},", b, fmt::join(descendants_, ", "));
  descendants_.push_back(b);
  b->add_ancestor(ir::block_ptr{this});
}

// Remove anything that does not have a consumer.
// We do this in reverse order so that eliminating one useless value can eliminate its inputs.
void block::remove_unused_operations() {
  reverse_remove_if(operations_, [](const ir::value_ptr v) {
    if (v->is_unused()) {
      v->remove();
      return true;
    }
    return false;
  });
}

// We traverse either upwards or downwards, recursively coloring nodes until we find a node
// that has already been colored - that is the intersection point. There might be more efficient
// ways to implement this, but we are doing relatively small searches.
ir::block_ptr find_merge_point(const ir::block_ptr left, const ir::block_ptr right,
                               const search_direction direction) {
  // queue with [node, color]
  std::deque<std::pair<ir::block_ptr, bool>> queue;
  queue.emplace_back(left, true);
  queue.emplace_back(right, false);

  std::unordered_map<ir::block_ptr, bool> visited;
  visited.reserve(20);

  while (!queue.empty()) {
    const auto [b, color] = queue.front();
    queue.pop_front();

    if (const auto [it, was_inserted] = visited.emplace(b, color);
        !was_inserted && it->second != color) {
      // Already visited by a different color, we found the intersection point:
      return b;
    }
    if (direction == search_direction::downwards) {
      for (ir::block_ptr child : b->descendants()) {
        queue.emplace_back(child, color);
      }
    } else {
      for (ir::block_ptr ancestor : b->ancestors()) {
        queue.emplace_back(ancestor, color);
      }
    }
  }

  WF_ASSERT_ALWAYS("All branches should have a merge point.");
}

}  // namespace wf::ir
