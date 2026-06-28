// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/control_flow_graph.h"

#include <deque>
#include <unordered_set>

namespace wf {

// Hash the integer names of a pair of blocks.
struct hash_block_pair {
  constexpr std::size_t operator()(const std::pair<std::size_t, std::size_t>& p) const noexcept {
    return hash_combine(p.first, p.second);
  }
};

// Convert ir that contains `cond` (ternary) statements into a control flow graph that contains
// jumps, blocks, and phi functions.
class ir_control_flow_converter {
 public:
  // Construct from existing control flow graph. The contents are moved, and `input` is left empty.
  explicit ir_control_flow_converter(control_flow_graph&& input, bool convert_ternaries);

  // Convert all ternary `cond` statements into conditionals, introducing new blocks and jump
  // statements in the process. Return a new control flow graph (consuming `this` in the process).
  control_flow_graph convert() &&;

 private:
  // True if node `v` has been visited.
  bool is_visited(ir::const_value_ptr v) const;

  // True if all downstream consumers of `v` have been visited.
  bool all_consumers_visited(ir::const_value_ptr val) const;

  // Queue any operands of `v` whose consumers have all been visited. When `speculative` is true we
  // are filling a conditionally-executed block, so operands that are unsafe to speculatively
  // evaluate (or that depend on such operations) are duplicated rather than hoisted into an outer
  // scope where they would be evaluated unconditionally.
  void queue_operands(std::deque<ir::value_ptr>& queue, ir::value_ptr v, bool speculative);

  // True if `v` is an operation that is unsafe to evaluate speculatively (eg. division, which may
  // divide by zero, or a function with a restricted domain such as `sqrt` or `acos`).
  static bool is_unsafe_to_speculate(ir::const_value_ptr v);

  // True if `v` is unsafe to speculate, or transitively depends on a value that is. Such values
  // must not be hoisted out of the conditional branch that guards them. Memoized in
  // `speculation_sensitive_`.
  bool is_speculation_sensitive(ir::const_value_ptr v);

  // Create a duplicate of `val` (same operation, type and operands). The clone is initially parented
  // to the (transient) input block, and is re-parented when it is processed.
  ir::value_ptr clone_value(ir::value_ptr val);

  // Process all non-conditional values in `queue` (which is modified in place).
  // When only conditionals are left, exit and return only the conditionals.
  std::vector<ir::value_ptr> process_non_conditionals(std::deque<ir::value_ptr>& queue,
                                                      std::vector<ir::value_ptr>& deferred,
                                                      ir::block_ptr output_block, bool speculative);

  // Process all the values in `queue`, and insert them into `output_block`. We iterate
  // until we hit conditionals that need to be traversed, at which juncture a branch
  // is inserted. Any values that cannot be processed are placed into `deferred`. `speculative`
  // indicates whether `output_block` lies on a conditionally-executed code path.
  ir::block_ptr process(std::deque<ir::value_ptr> queue, ir::block_ptr output_block,
                        std::vector<ir::value_ptr>& deferred, bool speculative);

  // Eliminate any useless copies we introduced in the process of conversion.
  void eliminate_useless_copies();

  // Clean up anything that is not referenced in the output:
  void discard_unused_input_values();

  // Allocate a new block.
  ir::block_ptr create_block();

  std::vector<ir::value::unique_ptr> values_;
  std::vector<ir::block::unique_ptr> blocks_;
  std::unordered_set<ir::const_value_ptr> visited_;
  std::unordered_map<std::pair<std::size_t, std::size_t>, bool, hash_block_pair> is_dominated_by_;
  std::unordered_map<ir::const_value_ptr, bool> speculation_sensitive_;
  ir::block::unique_ptr input_block_;
  bool convert_ternaries_;
};

}  // namespace wf
