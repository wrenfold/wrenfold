// Copyright 2024 Gareth Cross
#include "wf/code_generation/ir_block.h"

namespace wf::ir {

void block::replace_descendant(ir::block_ptr target, ir::block_ptr replacement) {
  WF_ASSERT_NOT_EQUAL(target, replacement);

  if (!operations.empty()) {
    if (const ir::value_ptr jump_val = operations.back(); jump_val->is_type<ir::jump_condition>()) {
      WF_ASSERT_EQUAL(2, descendants.size());
    } else {
      WF_ASSERT_GREATER_OR_EQ(1, descendants.size());
    }
  }

  const ir::block_ptr self{this};
  target->remove_ancestor(self);
  replacement->add_ancestor(self);

  const auto it = std::find(descendants.begin(), descendants.end(), target);
  WF_ASSERT(it != descendants.end());
  *it = replacement;
}

void block::add_ancestor(const block_ptr b) {
  WF_ASSERT(std::find(ancestors.begin(), ancestors.end(), b) == ancestors.end(),
            "Attempted to insert duplicate into ancestor list: {}", b->name);
  ancestors.push_back(b);
}

void block::remove_ancestor(const block_ptr b) {
  const auto it = std::find(ancestors.begin(), ancestors.end(), b);
  WF_ASSERT(it != ancestors.end(), "Block {} is not an ancestor of {}", b->name, name);
  ancestors.erase(it);
}

void block::add_descendant(const block_ptr b) {
  WF_ASSERT(std::find(descendants.begin(), descendants.end(), b) == descendants.end(),
            "Block {} already exists in descendants list: {},", b, fmt::join(descendants, ", "));
  descendants.push_back(b);
  b->add_ancestor(ir::block_ptr{this});
}

}  // namespace wf::ir
