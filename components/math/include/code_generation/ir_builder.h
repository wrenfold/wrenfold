// Copyright 2023 Gareth Cross
#pragma once
#include <unordered_map>
#include <vector>

#include "assertions.h"
#include "code_generation/expression_group.h"
#include "code_generation/ir_types.h"
#include "enumerations.h"
#include "expression.h"
#include "function_evaluator.h"
#include "hashing.h"

namespace math {
struct IRFormVisitor;

// Object for creating and manipulating our simple intermediate representation.
struct FlatIr {
 public:
  // Construct from a set of output expressions.
  explicit FlatIr(const std::vector<ExpressionGroup>& expressions);

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Eliminate duplicate operations. Most are eliminated during the conversion from expressions
  // to the IR.
  void eliminate_duplicates();

  // Number of operations:
  std::size_t num_operations() const;

  // Number of conditional statements.
  std::size_t num_condtionals() const;

  // Get the single block of operations.
  ir::BlockPtr get_block() const { return ir::BlockPtr{block_}; }

 protected:
  void strip_unused_values();

  // Single flat block:
  ir::Block::unique_ptr block_;

  // Owns all the instructions.
  std::vector<ir::Value::unique_ptr> values_;

  friend struct IRFormVisitor;
  friend struct OutputIr;
};

struct OutputIr {
 public:
  // Construct from `FlatIr` (which is cleared in the process).
  explicit OutputIr(FlatIr&& input);

  // Format IR for every value.
  std::string to_string() const;

  // Size of value numbers when printed (# digits).
  std::size_t value_print_width() const;

  // Number of operations in the IR.
  std::size_t num_operations() const;

  // Number of operations:
  std::size_t num_conditionals() const;

  // NUmber of blocks.
  std::size_t num_blocks() const { return blocks_.size(); }

  // Get the first block (start of execution).
  ir::BlockPtr first_block() const {
    const auto it =
        std::find_if(blocks_.begin(), blocks_.end(),
                     [](const ir::Block::unique_ptr& block) { return block->has_no_ancestors(); });
    ASSERT(it != blocks_.end(), "Must be an entry block");
    return ir::BlockPtr{*it};
  }

 private:
  // Allocate a new block
  ir::BlockPtr create_block();

  // Owns all the blocks.
  std::vector<ir::Block::unique_ptr> blocks_;

  // Owns all the instructions
  std::vector<ir::Value::unique_ptr> values_;

  friend struct IrConverter;
};

// Argument to FindMergePoints
enum class SearchDirection { Downwards, Upwards };

// Find the block where control flow merges after branching into left/right.
ir::BlockPtr find_merge_point(const ir::BlockPtr left, const ir::BlockPtr right,
                              const SearchDirection direction);

// Re-create `Expr` tree from the IR representation. For use in unit tests.
std::unordered_map<OutputKey, std::vector<Expr>, hash_struct<OutputKey>>
create_output_expression_map(ir::BlockPtr starting_block,
                             const std::unordered_map<std::string, bool>* output_arg_exists);

}  // namespace math
