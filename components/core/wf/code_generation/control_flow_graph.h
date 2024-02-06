// Copyright 2023 Gareth Cross
#pragma once
#include <vector>

#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/ir_types.h"
#include "wf/enumerations.h"

namespace wf {

// Store interemediate representation values and blocks. The blocks are arranged in a graph
// that describes the control flow in a code-generation function.
class control_flow_graph {
 public:
  // Construct from a set of output expressions.
  explicit control_flow_graph(const std::vector<expression_group>& groups);

  // Consume `this` and convert to a control flow graph that contains jumps.
  // This converts the graph from one with `cond` operations to one with multiple blocks and jump
  // operations.
  control_flow_graph convert_conditionals_to_control_flow() &&;

  // Eliminate duplicate operations. Most are eliminated during the conversion from expressions
  // to the IR, but the conversion process can introduce a few new duplicates.
  void eliminate_duplicates();

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Number of operations in the IR.
  std::size_t num_operations() const;

  // Number of operations:
  std::size_t num_conditionals() const;

  // Number of blocks.
  std::size_t num_blocks() const { return blocks_.size(); }

  // Get the first block (start of execution).
  ir::block_ptr first_block() const;

  // Count instances of operations matching predicate `Func`.
  template <typename Func>
  std::size_t count_operation(Func&& func) const {
    return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                           [&func](std::size_t total, const ir::block::unique_ptr& blk) {
                             return total + blk->count_operation(func);
                           });
  }

  // Count instances of operations of type `T`.
  template <typename T>
  std::size_t count_operation() const {
    return count_operation([](const T&) constexpr { return true; });
  }

  // Count instances of a function call.
  std::size_t count_function(std_math_function enum_value) const noexcept;

 private:
  control_flow_graph(std::vector<ir::block::unique_ptr> blocks,
                     std::vector<ir::value::unique_ptr> values)
      : blocks_(std::move(blocks)), values_(std::move(values)) {}

  // Remove any values without consumers (that are not endpoints like `ir::save`).
  void strip_unused_values();

  // Owns all the blocks.
  std::vector<ir::block::unique_ptr> blocks_;

  // Owns all the instructions
  std::vector<ir::value::unique_ptr> values_;

  friend class ir_form_visitor;
  friend class ir_control_flow_converter;
};

}  // namespace wf
