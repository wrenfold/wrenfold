// Copyright 2023 Gareth Cross
#pragma once
#include <algorithm>
#include <array>
#include <typeindex>
#include <unordered_map>
#include <variant>
#include <vector>

#include "ast.h"
#include "enumerations.h"
#include "expression.h"
#include "function_evaluator.h"
#include "hashing.h"
#include "non_null_ptr.h"

namespace math {
namespace ir {

class Value;
class Block;
struct IRFormVisitor;

using ValuePtr = NonNullPtr<ir::Value>;
using BlockPtr = NonNullPtr<ir::Block>;

// A block of operations:
class Block {
 public:
  using unique_ptr = std::unique_ptr<Block>;

  // Construct w/ counter.
  explicit Block(std::size_t name) : name(name) {}

  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  // TODO: There is an argument to be made that this should be an intrusive double-linked-list.
  // For now I'm going with pointers, since this is simpler to get started, even though it means
  // we'll have some O(N) operations (for moderately small N).
  std::vector<ValuePtr> operations;

  // Ancestor blocks (blocks that preceded this one).
  std::vector<BlockPtr> ancestors;

  // True if the block has no operations.
  bool IsEmpty() const { return operations.empty(); }

  // True if the block has no ancestors.
  bool IsUnreachable() const { return ancestors.empty(); }

  template <typename Callable>
  void VisitSuccessors(Callable&& callable) const;

  void AddAncestor(BlockPtr b);

  void RemoveAncestor(BlockPtr b);

  void PushValue(const ValuePtr& v);
};

struct Add {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "add"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Add&) const { return true; }
};

struct Mul {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "mul"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Mul&) const { return true; }
};

struct Pow {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "pow"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Pow&) const { return true; }
};

struct Copy {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "copy"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Copy&) const { return true; }
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return math::ToString(name); }
  constexpr std::size_t Hash() const { return static_cast<std::size_t>(name); }
  constexpr bool IsSame(const CallUnaryFunc& other) const { return name == other.name; }

  CallUnaryFunc(UnaryFunctionName name) : name(name) {}

  UnaryFunctionName name;
};

struct Cond {
  constexpr static int NumValueOperands() { return 3; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "cond"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Cond&) const { return true; }
};

struct Phi {
  constexpr static int NumValueOperands() { return 3; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "phi"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Phi&) const { return true; }
};

struct Compare {
  constexpr static int NumValueOperands() { return 2; }
  constexpr static bool IsCommutative() { return false; }

  constexpr std::string_view ToString() const {
    switch (operation) {
      case RelationalOperation::LessThan:
        return "lt";
      case RelationalOperation::LessThanOrEqual:
        return "lte";
      case RelationalOperation::Equal:
        return "eq";
    }
    return "<NOT A VALID ENUM VALUE>";
  }

  constexpr std::size_t Hash() const { return static_cast<std::size_t>(operation); }
  constexpr bool IsSame(const Compare& comp) const { return operation == comp.operation; }

  explicit Compare(const RelationalOperation operation) : operation(operation) {}

  RelationalOperation operation;
};

struct Load {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "load"; }
  std::size_t Hash() const { return HashExpression(expr); }
  bool IsSame(const Load& other) const { return expr.IsIdenticalTo(other.expr); }

  explicit Load(Expr expr) : expr(std::move(expr)) {}

  Expr expr;
};

struct Jump {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "jump"; }
  std::size_t Hash() const { return next_->name; }
  bool IsSame(const Jump& other) const { return next_ == other.next_; }

  explicit Jump(BlockPtr block) : next_{block} {}

  BlockPtr Next() const { return next_; }

 private:
  BlockPtr next_;
};

struct ConditionalJump {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "jump"; }

  // Hash takes account of which block we are jumping to.
  std::size_t Hash() const { return HashCombine(next_true_->name, next_false_->name); }

  // Blocks must match.
  bool IsSame(const ConditionalJump& other) const {
    return next_true_ == other.next_true_ && next_false_ == other.next_false_;
  }

  ConditionalJump(BlockPtr next_true, BlockPtr next_false)
      : next_true_(next_true), next_false_(next_false) {}

  BlockPtr NextTrue() const { return next_true_; }
  BlockPtr NextFalse() const { return next_false_; }

 private:
  BlockPtr next_true_;
  BlockPtr next_false_;
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, Mul, Pow, Copy, CallUnaryFunc, Compare, Cond, Phi, Load, Jump,
                               ConditionalJump>;

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class Value {
 public:
  using unique_ptr = std::unique_ptr<Value>;

  // Construct:
  template <typename OpType>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation,
                 std::vector<ValuePtr>&& operands)
      : name_(name), parent_(parent), op_(std::move(operation)), operands_(std::move(operands)) {
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  // Construct with input values specified as variadic arg list.
  template <typename OpType, typename... Args,
            typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Args, ValuePtr>...>>>
  explicit Value(uint32_t name, ir::BlockPtr parent, OpType&& operation, Args... args)
      : Value(name, parent, std::move(operation), MakeValueVector(args...)) {}

  // Access underlying integer.
  constexpr uint32_t Name() const { return name_; }

  // Get the parent block.
  ir::BlockPtr Parent() const { return parent_; }

  // Set the parent pointer.
  void SetParent(ir::BlockPtr b) { parent_ = b; }

  // Access underlying operation
  const Operation& Op() const { return op_; }

  // True if the underlying operation is `T`.
  template <typename T>
  bool Is() const {
    return std::holds_alternative<T>(op_);
  }

  // Cast the operation to the specified type.
  template <typename T>
  const T& As() const {
    return std::get<T>(op_);
  }

  // True if this is a jump.
  bool IsJump() const { return Is<ir::ConditionalJump>() || Is<ir::Jump>(); }

  // True if this is a phi function.
  bool IsPhi() const { return Is<ir::Phi>(); }

  // True if any values that consume this one are phi functions.
  bool IsConsumedByPhi() const {
    return std::any_of(consumers_.begin(), consumers_.end(), [this](const ValuePtr& v) {
      return v->IsPhi() && v->Operands().front() != this;
    });
  }

  // Replace an operand to this instruction with another.
  void ReplaceOperand(const ValuePtr old, const ValuePtr replacement) {
    auto it = std::find(operands_.begin(), operands_.end(), old);
    ASSERT(it != operands_.end(), "Tried to replace missing operand: {}", old->Name());
    *it = replacement;
    replacement->AddConsumer(this);
    MaybeSortOperands();
  }

  // Change the underlying operation that computes this value.
  template <typename OpType, typename... Args>
  void SetOp(OpType&& op, Args... args) {
    // Notify existing operands we no longer reference them
    for (const ValuePtr& operand : operands_) {
      operand->RemoveConsumer(this);
    }
    // Record new operands:
    operands_.clear();
    (operands_.push_back(args), ...);
    op_ = std::move(op);
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  void AddConsumer(ir::Value* const v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    ValuePtr val{v};
    ASSERT(it == consumers_.end(), "Duplicate insertion of value: {}", val->Name());
    consumers_.push_back(val);
  }

  void RemoveConsumer(ir::Value* v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    ASSERT(v);
    ASSERT(it != consumers_.end(), "Tried to remove non-existent consumer: {}", v->Name());
    fmt::print("Removing consumer {} from {}\n", ir::ValuePtr{v}, ir::ValuePtr{this});
    consumers_.erase(it);
  }

  // Access instruction operands.
  const std::vector<ValuePtr>& Operands() const { return operands_; }

  // Get the first operand.
  const ValuePtr& Front() const { return operands_.front(); }

  // Access all consumers of this value.
  const std::vector<ValuePtr>& Consumers() const { return consumers_; }

  // True if `this` accepts `v` as an operand.
  bool Consumes(const ValuePtr& v) const {
    return std::any_of(operands_.begin(), operands_.end(),
                       [&](const ValuePtr& operand) { return operand == v; });
  }

  // True if operands to values match.
  bool OperandsMatch(const ValuePtr& other) const {
    if (operands_.size() != other->operands_.size()) {
      return false;
    }
    return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
  }

  // Replace this value w/ the argument.
  // All downstream consumers have their arguments swapped, and the consumer list is cleared.
  void ReplaceWith(const ValuePtr other);

  // Nuke this value. Only valid if it is not consumed.
  void Remove();

  // Update any phi functions that consume this value (as though this value were no longer
  // computed).
  void RemoveFromDownstreamPhiFunctions();

  // True if there are no consumers of this value.
  bool IsUnused() const { return !IsJump() && consumers_.empty(); }

 protected:
  // Remove an operand from the operands vector.
  void RemoveOperand(ir::Value* v);

  void MaybeSortOperands() {
    const bool commutative = std::visit([](const auto& op) { return op.IsCommutative(); }, op_);
    if (commutative) {
      SortOperands();
    }
  }

  void SortOperands() {
    std::sort(operands_.begin(), operands_.end(),
              [](const ValuePtr& a, const ValuePtr& b) { return a->Name() < b->Name(); });
  }

  template <typename OpType>
  void CheckNumOperands() {
    constexpr int expected_num_args = OpType::NumValueOperands();
    if constexpr (expected_num_args >= 0) {
      ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  void NotifyOperands() {
    for (const ValuePtr& operand : operands_) {
      operand->AddConsumer(this);
    }
  }

  template <typename... Args>
  std::vector<ValuePtr> MakeValueVector(Args&&... args) {
    std::vector<ValuePtr> vec{};
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
  }

  // Unique name for this value (used for formatting variable names).
  uint32_t name_;

  // Parent block
  ir::BlockPtr parent_;

  // The operation that computes this value.
  Operation op_;

  // Operands to the operation. Maybe be empty for some operations.
  // TODO: Small vector of size ~4:
  std::vector<ValuePtr> operands_;

  // Downstream values that consume this one:
  std::vector<ValuePtr> consumers_;
};

}  // namespace ir

// Object for creating the intermediate representation. The IR is then given to the code-generator
// to be simplified.
struct IrBuilder {
 public:
  // ConstructMatrix from a set of output expressions.
  explicit IrBuilder(const std::vector<Expr>& expressions);

  // Get the values indices for the outputs.
  const std::vector<ir::ValuePtr>& OutputValues() const { return output_values_; }

  // Format IR for every value.
  std::string ToString(bool print_jump_origins = false) const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  // Recreate the expression tree for all the output expressions.
  std::vector<Expr> CreateOutputExpressions() const;

  // Create AST to be emitted.
  std::vector<ast::Variant> CreateAST(const ast::FunctionSignature& description);

  // Eliminate duplicated operations.
  void EliminateDuplicates();
  void StripUnusedValues();
  void FoldConditionalJumps();
  void EliminateUnreachableBlocks();
  void CombineSequentialBlocks();
  void LiftValues();
  void DropValues();
  void EliminateChainedJumps();

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

  // The output values, one per input expression.
  std::vector<ir::ValuePtr> output_values_;

  // Next available value name, starting at zero.
  uint32_t insertion_point_{0};

  // Dead blocks:
  std::vector<ir::Block::unique_ptr> dead_blocks_;

  friend struct ir::IRFormVisitor;
};

}  // namespace math
