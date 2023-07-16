// Copyright 2023 Gareth Cross
#pragma once
#include <algorithm>
#include <array>
#include <typeindex>
#include <unordered_map>
#include <variant>
#include <vector>

#include "ast.h"
#include "code_generation/ir_types.h"
#include "enumerations.h"
#include "expression.h"
#include "function_evaluator.h"
#include "hashing.h"

namespace math {

struct IRFormVisitor;

// Object for creating the intermediate representation. The IR is then given to the code-generator
// to be simplified.
struct IrBuilder {
 public:
  // Construct from a set of output expressions.
  explicit IrBuilder(const std::vector<ExpressionGroup>& expressions);

  // Format IR for every value.
  std::string ToString(bool print_jump_origins = false) const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  // Recreate the expression tree for all the output expressions.
  std::unordered_map<OutputKey, std::vector<Expr>, OutputKeyHasher> CreateOutputExpressions() const;

  ast::FunctionDefinition CreateAST(const ast::FunctionSignature& signature) const;

  // Eliminate duplicated operations.
  void EliminateDuplicates();
  void StripUnusedValues();
  void ConvertTernaryConditionalsToJumps();
  void ReorderConditionalsInBlock(const ir::BlockPtr block);

  void DropValues();

  // Number of operations:
  std::size_t NumOperations() const;

  // Number of conditional jumps.
  std::size_t NumJumps() const;

 protected:
  ir::BlockPtr FirstBlock() const {
    ASSERT(!blocks_.empty());
    return ir::BlockPtr{blocks_.front()};
  }

  // Allocate new value and insert it into block.
  template <typename OpType, typename... Args>
  ir::ValuePtr CreateOperation(ir::BlockPtr block, OpType&& op, Args... args);

  // Allocate a new block
  ir::BlockPtr CreateBlock();

  // Owns all the blocks.
  std::vector<ir::Block::unique_ptr> blocks_;

  // Owns all the instructions.
  std::vector<ir::Value::unique_ptr> values_;

  // Next available value name, starting at zero.
  uint32_t insertion_point_{0};

  // Dead blocks:
  std::vector<ir::Block::unique_ptr> dead_blocks_;

  friend struct IRFormVisitor;
};

}  // namespace math
