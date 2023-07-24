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
  std::string ToString() const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  // Eliminate duplicate operations. Most are eliminated during the conversion from expressions
  // to the IR.
  void EliminateDuplicates();

  // Number of operations:
  std::size_t NumOperations() const;

  std::size_t NumConditionals() const;

  // Get the single block of operations.
  ir::BlockPtr GetBlock() const { return ir::BlockPtr{block_}; }

 protected:
  void StripUnusedValues();

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
  std::string ToString() const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  // Number of operations in the IR.
  std::size_t NumOperations() const;

  // Number of operations:
  std::size_t NumConditionals() const;

  // NUmber of blocks.
  std::size_t NumBlocks() const { return blocks_.size(); }

  // Get the first block (start of execution).
  ir::BlockPtr FirstBlock() const {
    const auto it =
        std::find_if(blocks_.begin(), blocks_.end(),
                     [](const ir::Block::unique_ptr& block) { return block->HasNoAncestors(); });
    ASSERT(it != blocks_.end(), "Must be an entry block");
    return ir::BlockPtr{*it};
  }

 private:
  // Allocate a new block
  ir::BlockPtr CreateBlock();

  // Owns all the blocks.
  std::vector<ir::Block::unique_ptr> blocks_;

  // Owns all the instructions
  std::vector<ir::Value::unique_ptr> values_;

  friend struct IrConverter;
};

// Argument to FindMergePoints
enum class SearchDirection { Downwards, Upwards };

// Find the block where control flow merges after branching into left/right.
ir::BlockPtr FindMergePoint(const ir::BlockPtr left, const ir::BlockPtr right,
                            const SearchDirection direction);

// Re-create `Expr` tree from the IR representation. For use in unit tests.
std::unordered_map<OutputKey, std::vector<Expr>, OutputKeyHasher> CreateOutputExpressionMap(
    ir::BlockPtr starting_block, const std::unordered_map<std::size_t, bool>* output_arg_exists);

}  // namespace math
