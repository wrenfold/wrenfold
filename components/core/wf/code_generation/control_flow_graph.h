// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <vector>

#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/ir_block.h"
#include "wf/code_generation/operation_counts.h"
#include "wf/enumerations.h"

namespace wf {

// Params governing how expressions are simplified/optimized.
struct optimization_params {
  // If > 0, automatically factorize sums of products.
  // The value determines how many passes we make through the graph to search for opportunities.
  std::size_t factorization_passes{3};
  // Convert additions and multiplications to binary operations.
  bool binarize_operations{true};
};

// Store intermediate representation values and blocks. The blocks are arranged in a graph
// that describes the control flow in a code-generation function.
// TODO: I tend to think the structure of this object could be improved:
//  - The use of "standard blocks" potentially overcomplicates things. The original motivation was
//    to learn from examples in compilers, but we only really need if-else support for now.
//  - I dislike that objects are connected via non-owning pointers. Maybe indices into an array
//    would be safer. Each `ir::value` object is fairly large, and could could potentially be split
//    into a struct-of-arrays design.
class control_flow_graph {
 public:
  // Construct from a set of output expressions.
  explicit control_flow_graph(const std::vector<expression_group>& groups,
                              const std::optional<optimization_params>& params = std::nullopt);

  // Consume `this` and convert to a control flow graph that contains jumps.
  // This converts the graph from one with `cond` operations to one with multiple blocks and jump
  // operations.
  control_flow_graph convert_conditionals_to_control_flow() &&;

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Number of operations in the IR.
  std::size_t num_operations() const;

  // Number of operations:
  std::size_t num_conditionals() const;

  // Number of blocks.
  std::size_t num_blocks() const noexcept { return blocks_.size(); }

  // Get the first block (start of execution).
  ir::block_ptr first_block() const;

  // Count instances of operations of type `T`.
  template <typename T>
  std::size_t count_operation() const {
    return accumulate_over_blocks([](const ir::const_block_ptr blk) {
      return blk->count_operation([](const T&) constexpr { return 1; });
    });
  }

  // Count instances of a function call.
  std::size_t count_function(std_math_function enum_value) const noexcept;

  // Count add operations.
  std::size_t count_additions() const noexcept;

  // Count multiplication operations.
  std::size_t count_multiplications() const noexcept;

  // Generate a summary of operations.
  operation_counts compute_operation_counts() const;

  // For debug builds and tests, check that invariants are satisfied.
  void assert_invariants() const;

  // Apply simplifications to all the operations in the graph.
  void apply_simplifications(const optimization_params& p);

  // Factorize sum-of-product expressions to reduce multiplication operations.
  void factorize_sums(std::size_t num_passes);

 private:
  template <typename Func>
  std::size_t accumulate_over_blocks(Func&& func) const {
    return std::accumulate(blocks_.begin(), blocks_.end(), static_cast<std::size_t>(0),
                           [&func](const std::size_t total, const ir::block::unique_ptr& b) {
                             return total +
                                    static_cast<std::size_t>(func(ir::const_block_ptr{b.get()}));
                           });
  }

  // Eliminate duplicate operations. Most are eliminated during the conversion from expressions
  // to the IR, but the conversion process can introduce a few new duplicates.
  void eliminate_duplicates();

  // Eliminate copy operations that other steps inserted.
  void eliminate_needless_copies();

  // Where duplicate sub-expressions can be found, break additions and multiplications into smaller
  // binary additions and multiplications. Common terms are extracted into temporaries.
  template <typename OpType>
  void binarize_operations(ir::block_ptr block);

  ir::value_ptr factorize_sum_of_products(const class variable_index_assignor& index_assignor,
                                          const class factorization& fac, ir::block_ptr block,
                                          absl::Span<const ir::value_ptr> muls,
                                          absl::Span<const ir::value_ptr> non_muls,
                                          std::vector<ir::value_ptr>& operations_out);

  // Convert sums of products into products by factorizing out common terms.
  void factorize_sums_in_block(ir::block_ptr block);

  // Merge nested multiplications together.
  void merge_multiplications_in_block(ir::block_ptr block);

  // Replace multiplications by -1 with negations.
  void insert_negations(ir::block_ptr block);

  // Push a new value into `values_`.
  template <typename T, typename... Args>
  ir::value_ptr push_value(ir::block_ptr block, T op, ir::value::types type, Args&&... args);

  // If the numeric type of `val` does not match `type`, insert a cast.
  ir::value_ptr maybe_cast(ir::value_ptr val, code_numeric_type type);

  control_flow_graph(std::vector<ir::block::unique_ptr> blocks,
                     std::vector<ir::value::unique_ptr> values);

  // Owns all the blocks.
  std::vector<ir::block::unique_ptr> blocks_;

  // Owns all the instructions
  std::vector<ir::value::unique_ptr> values_;

  friend class ir_form_visitor;
  friend class ir_control_flow_converter;
};

}  // namespace wf
