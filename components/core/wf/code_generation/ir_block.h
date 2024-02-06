// Copyright 2024 Gareth Cross
#pragma once
#include <algorithm>
#include <memory>
#include <vector>

#include "wf/code_generation/ir_value.h"

namespace wf::ir {

// A block of operations:
class block {
 public:
  using unique_ptr = std::unique_ptr<block>;

  // Construct w/ counter.
  explicit block(const std::size_t name) noexcept : name(name) {}

  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  // TODO: There is an argument to be made that this should be an intrusive double-linked-list.
  // For now I'm going with vec of pointers, since this is simpler to get started, even though it
  // means we'll have some O(N) operations (for moderately small N).
  std::vector<value_ptr> operations{};

  // Ancestor blocks (blocks that preceded this one).
  std::vector<block_ptr> ancestors{};

  // Descendants of this block (blocks we jump to from this one).
  std::vector<block_ptr> descendants{};

  // True if the block has no operations.
  bool is_empty() const noexcept { return operations.empty(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool has_no_ancestors() const noexcept { return ancestors.empty(); }

  // True if this block has no descendents.
  bool has_no_descendents() const noexcept { return descendants.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void replace_descendant(ir::block_ptr target, ir::block_ptr replacement);

  // Insert block into `ancestors` vector.
  void add_ancestor(block_ptr b);

  // Remove block from `ancestors` vector.
  void remove_ancestor(block_ptr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void add_descendant(ir::block_ptr b);

  // Count instances of operation of type `T`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const;
};

// Count instances of operation of type `T`.
// Defined here so that we can use methods on `value`.
template <typename Func>
std::size_t block::count_operation(Func&& func) const {
  return std::count_if(operations.begin(), operations.end(),
                       [&func](const ir::value_ptr v) -> bool {
                         return std::visit(
                             [&func](const auto& op) -> bool {
                               using arg_type = std::decay_t<decltype(op)>;
                               if constexpr (is_invocable_v<Func, const arg_type&>) {
                                 return func(op);
                               } else {
                                 return false;
                               }
                             },
                             v->value_op());
                       });
}

}  // namespace wf::ir
