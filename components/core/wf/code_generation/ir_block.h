// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <algorithm>
#include <memory>
#include <vector>

#include "wf/code_generation/ir_value.h"

namespace wf::ir {

// A block of operations in the control flow graph.
class block {
 public:
  using unique_ptr = std::unique_ptr<block>;

  // Construct w/ name.
  explicit block(const std::size_t name) noexcept : name_(name) {}

  // Unique number assigned to the block to identify it.
  constexpr std::size_t name() const noexcept { return name_; }

  // True if the block has no operations.
  bool is_empty() const noexcept { return operations_.empty(); }

  // Number of operations.
  std::size_t size() const noexcept { return operations_.size(); }

  // True if this block has no ancestors (typically only true for the starting block).
  bool has_no_ancestors() const noexcept { return ancestors_.empty(); }

  // True if this block has no descendents.
  bool has_no_descendents() const noexcept { return descendants_.empty(); }

  // Replace descendant `target` w/ `replacement`.
  void replace_descendant(ir::block_ptr target, ir::block_ptr replacement);

  // Insert block into `ancestors` vector.
  void add_ancestor(block_ptr b);

  // Remove block from `ancestors` vector.
  void remove_ancestor(block_ptr b);

  // Add `b` as a descendant, and add `this` as an ancestor of b.
  void add_descendant(ir::block_ptr b);

  // Push a new operation at the end of the block.
  const value_ptr& push(const ir::value_ptr val) {
    operations_.push_back(val);
    return operations_.back();
  }

  // Insert at the front of the operation vector.
  template <typename Iterator>
  void insert_front(Iterator begin, Iterator end) {
    operations_.insert(operations_.begin(), begin, end);
  }

  // Remove all operations that match predicate `func`.
  template <typename Func>
  void remove_operation_if(Func func) {
    operations_.erase(std::remove_if(operations_.begin(), operations_.end(), std::move(func)),
                      operations_.end());
  }

  // Remove all operations that match predicate `func`, traversing in reverse order.
  template <typename Func>
  void reverse_remove_operation_if(Func func) {
    // Somewhat lazy: Reverse the operations so that we can use forward iterator, then reverse back.
    std::reverse(operations_.begin(), operations_.end());
    operations_.erase(std::remove_if(operations_.begin(), operations_.end(), std::move(func)),
                      operations_.end());
    std::reverse(operations_.begin(), operations_.end());
  }

  // Get the last operation in the block.
  value_ptr last_operation() const {
    WF_ASSERT(!is_empty(), "Block is empty: {}", name_);
    return operations_.back();
  }

  // Count instances of operation of type `T`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const;

  // Access operations.
  constexpr const auto& operations() const noexcept { return operations_; }

  // Access descendents.
  constexpr const auto& descendants() const noexcept { return descendants_; }

  // Access ancestors.
  constexpr const auto& ancestors() const noexcept { return ancestors_; }

 private:
  // Unique number to refer to the block (a counter).
  std::size_t name_;

  // All the operations in the block, in order.
  // TODO: There is an argument to be made that this should be an intrusive double-linked-list.
  // For now I'm going with vec of pointers, since this is simpler to get started, even though it
  // means we'll have some O(N) operations (for moderately small N).
  std::vector<value_ptr> operations_{};

  // Ancestor blocks (blocks that preceded this one).
  std::vector<block_ptr> ancestors_{};

  // Descendants of this block (blocks we jump to from this one).
  std::vector<block_ptr> descendants_{};
};

// Create an `ir::value` in the specified block using operation `OpType`.
template <typename OpType, typename... Args>
ir::value_ptr create_operation(std::vector<ir::value::unique_ptr>& values,
                               const ir::block_ptr block, OpType&& op, ir::value::types type,
                               Args&&... args) {
  // Create a new value:
  const uint32_t name = values.empty() ? 0 : values.back()->name() + 1;
  std::unique_ptr<ir::value> value = std::make_unique<ir::value>(
      name, block, std::forward<OpType>(op), std::move(type), std::forward<Args>(args)...);
  // Insert in the provided block:
  const ir::value_ptr& last = block->push(value.get());
  // This is owned by the `values` vector:
  values.push_back(std::move(value));
  return last;
}

// Argument to `find_merge_point`.
enum class search_direction {
  // Search descendents.
  downwards,
  // Search ancestors (reverse order of execution).
  upwards
};

// Find the block where control flow merges after branching into left/right.
// Depending on the value of `direction`, we search either anscestors or descendents.
ir::block_ptr find_merge_point(ir::block_ptr left, ir::block_ptr right, search_direction direction);

// Count instances of operation of type `T`.
// Defined here so that we can use methods on `value`.
template <typename Func>
std::size_t block::count_operation(Func&& func) const {
  if constexpr (is_invocable_v<Func, ir::value_ptr>) {
    return std::count_if(operations_.begin(), operations_.end(), std::forward<Func>(func));
  } else {
    return std::count_if(operations_.begin(), operations_.end(),
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
}

}  // namespace wf::ir

// Formatter for pointer to block
template <>
struct fmt::formatter<wf::ir::block_ptr, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::ir::block_ptr x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "block_{}", x->name());
  }
};
