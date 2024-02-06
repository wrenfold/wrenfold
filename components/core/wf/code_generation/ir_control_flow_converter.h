// Copyright 2024 Gareth Cross
#pragma once
#include "wf/code_generation/flat_ir.h"

#include <deque>
#include <unordered_set>

namespace wf {

// Convert ir that contains `cond` (ternary) statements into a control flow graph that contains
// jumps, blocks, and phi functions.
class ir_control_flow_converter {
 public:
  // Construct from existing control flow graph. The contents are moved, and `input` is left empty.
  explicit ir_control_flow_converter(control_flow_graph&& input);

  // Convert all ternary `cond` statements into conditionals, introducing new blocks and jump
  // statements in the process. Return a new control flow graph (consuming `this` in the process).
  control_flow_graph convert() &&;

 private:
  // True if node `v` has been visited.
  bool is_visited(ir::value_ptr v) const;

  // True if all downstream consumers of `v` have been visited.
  bool all_consumers_visited(ir::value_ptr val) const;

  // Queue any operands of `v` whose consumers have all been visited.
  void queue_operands(std::deque<ir::value_ptr>& queue, ir::value_ptr v) const;

  // Process all non-conditional values in `queue` (which is modified in place).
  // When only conditionals are left, exit and return only the conditionals.
  std::vector<ir::value_ptr> process_non_conditionals(std::deque<ir::value_ptr>& queue,
                                                      std::vector<ir::value_ptr>& deferred,
                                                      ir::block_ptr output_block);

  // Process all the values in `queue`, and insert them into `output_block`. We iterate
  // until we hit conditionals that need to be traveresed, at which juncture a branch
  // is inserted. An values that cannot be processed are placed into `deferred`.
  ir::block_ptr process(std::deque<ir::value_ptr> queue, ir::block_ptr output_block,
                        std::vector<ir::value_ptr>& deferred);

  // Eliminate any useless copies we introduced in the process of conversion.
  void eliminate_useless_copies();

  // Clean up anything that is not referenced in the output:
  void discard_unused_input_values();

  // Allocate a new block.
  ir::block_ptr create_block();

  std::vector<ir::value::unique_ptr> values_;
  std::vector<ir::block::unique_ptr> blocks_{};
  std::unordered_set<ir::value_ptr> visited_{};
  ir::block::unique_ptr input_block_;
};

}  // namespace wf
