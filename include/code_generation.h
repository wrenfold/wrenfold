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

namespace math {
namespace ir {

class Value;
struct IRFormVisitor;

// A block of operations:
class Block {
 public:
  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  std::vector<ir::Value*> operations;

  // Ancestor blocks (blocks that preceded this one).
  std::vector<ir::Block*> ancestors;

  // True if the block has no operations.
  bool IsEmpty() const { return operations.empty(); }

  // True if the block has no ancestors.
  bool IsUnreachable() const { return ancestors.empty(); }

  template <typename Callable>
  void VisitSuccessors(Callable&& callable) const;

  void AddAncestor(ir::Block* const b);

  void RemoveAncestor(ir::Block* const b);

  void PushValue(ir::Value* const v);
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

  explicit Jump(Block* block) : next_{block} { ASSERT(block); }

  ir::Block* Next() const { return next_; }

 private:
  ir::Block* next_;
};

struct ConditionalJump {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "jump"; }
  std::size_t Hash() const { return HashCombine(next_true_->name, next_false_->name); }
  bool IsSame(const ConditionalJump& other) const {
    return next_true_ == other.next_true_ && next_false_ == other.next_false_;
  }

  ConditionalJump(Block* next_true, Block* next_false)
      : next_true_(next_true), next_false_(next_false) {
    ASSERT(next_true);
    ASSERT(next_false);
  }

  ir::Block* NextTrue() const { return next_true_; }
  ir::Block* NextFalse() const { return next_false_; }

 private:
  ir::Block* next_true_;
  ir::Block* next_false_;
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, Mul, Pow, Copy, CallUnaryFunc, Compare, Cond, Phi, Load, Jump,
                               ConditionalJump>;

// Values are the result of any instruction we store in the IR.
// All values have a name (an integer), and an operation that computed them.
// Values may be used as operands to other operations.
class Value {
 public:
  // Construct:
  template <typename OpType>
  explicit Value(uint32_t name, ir::Block* parent, OpType&& operation,
                 std::vector<ir::Value*>&& operands)
      : name_(name), parent_(parent), op_(std::move(operation)), operands_(std::move(operands)) {
    ASSERT(parent);
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  // Construct with input values specified as variadic arg list.
  template <
      typename OpType, typename... Args,
      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Args, ir::Value*>...>>>
  explicit Value(uint32_t name, ir::Block* parent, OpType&& operation, Args... args)
      : Value(name, parent, std::move(operation), MakeValueVector(args...)) {}

  // Access underlying integer.
  constexpr uint32_t Name() const { return name_; }

  // Get the parent block.
  ir::Block* Parent() const { return parent_; }

  // Set the parent pointer.
  void SetParent(ir::Block* b) {
    ASSERT(b);
    parent_ = b;
  }

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
    return std::any_of(consumers_.begin(), consumers_.end(), [this](ir::Value* v) {
      return v->IsPhi() && v->Operands().front() != this;
    });
  }

  // Replace an operand to this instruction with another.
  void ReplaceOperand(ir::Value* const old, ir::Value* const replacement) {
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
    for (ir::Value* const operand : operands_) {
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
    ASSERT(v);
    ASSERT(it == consumers_.end(), "Duplicate insertion of value: {}", v->Name());
    consumers_.push_back(v);
  }

  void RemoveConsumer(ir::Value* const v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    ASSERT(v);
    ASSERT(it != consumers_.end(), "Tried to remove non-existent consumer: {}", v->Name());
    consumers_.erase(it);
  }

  // Access instruction operands.
  const std::vector<ir::Value*>& Operands() const { return operands_; }

  // Get the first operand.
  ir::Value* Front() const { return operands_.front(); }

  // Access all consumers of this value.
  const std::vector<ir::Value*>& Consumers() const { return consumers_; }

  // True if `this` accepts `v` as an operand.
  bool Consumes(ir::Value* const v) const {
    return std::any_of(operands_.begin(), operands_.end(),
                       [&](ir::Value* operand) { return operand == v; });
  }

  // True if operands to values match.
  bool OperandsMatch(const ir::Value* other) const {
    ASSERT(other);
    if (operands_.size() != other->operands_.size()) {
      return false;
    }
    return std::equal(operands_.begin(), operands_.end(), other->operands_.begin());
  }

  // Replace this value w/ the argument.
  void ReplaceWith(ir::Value* const other) {
    ASSERT(other);
    ASSERT(this != other);
    for (ir::Value* const consumer : consumers_) {
      consumer->ReplaceOperand(this, other);
    }
    consumers_.clear();
  }

  // Nuke this value. Only valid if the only consumers are phi functions.
  void Remove() {
    // Notify our operands we no longer consume them.
    for (ir::Value* const operand : operands_) {
      operand->RemoveConsumer(this);
    }
    // Check downstream consumers:
    for (ir::Value* const consumer : consumers_) {
      ASSERT(consumer->IsPhi(), "Consumers must be phi functions to remove this instruction");
      // Keep the operand to the phi function that is not `this`:
      const std::vector<ir::Value*>& phi_args = consumer->Operands();
      ASSERT_EQUAL(3, phi_args.size());
      consumer->ReplaceWith(phi_args[1] == this ? phi_args[2] : phi_args[1]);
    }
    parent_ = nullptr;
  }

  // True if there are no consumers of this value.
  bool IsUnused() const { return !IsJump() && consumers_.empty(); }

 protected:
  void MaybeSortOperands() {
    const bool commutative = std::visit([](const auto& op) { return op.IsCommutative(); }, op_);
    if (commutative) {
      SortOperands();
    }
  }

  void SortOperands() {
    std::sort(operands_.begin(), operands_.end(),
              [](ir::Value* a, ir::Value* b) { return a->Name() < b->Name(); });
  }

  template <typename OpType>
  void CheckNumOperands() {
    constexpr int expected_num_args = OpType::NumValueOperands();
    if constexpr (expected_num_args >= 0) {
      ASSERT_EQUAL(static_cast<std::size_t>(expected_num_args), operands_.size());
    }
  }

  void NotifyOperands() {
    for (ir::Value* const operand : operands_) {
      ASSERT(operand);
      operand->AddConsumer(this);
    }
  }

  template <typename... Args>
  std::vector<ir::Value*> MakeValueVector(Args... args) {
    std::vector<ir::Value*> vec{};
    vec.reserve(sizeof...(Args));
    (vec.push_back(std::forward<Args>(args)), ...);
    return vec;
  }

  // Unique name for this value (used for formatting variable names).
  uint32_t name_;

  // Parent block
  ir::Block* parent_;

  // The operation that computes this value.
  Operation op_;

  // Operands to the operation. Maybe be empty for some operations.
  // TODO: Small vector of size ~4:
  std::vector<ir::Value*> operands_;

  // Downstream values that consume this one:
  std::vector<ir::Value*> consumers_;
};

}  // namespace ir

// Object for creating the intermediate representation. The IR is then given to the code-generator
// to be simplified.
struct IrBuilder {
 public:
  // ConstructMatrix from a set of output expressions.
  explicit IrBuilder(const std::vector<Expr>& expressions);

  // Get the values indices for the outputs.
  const std::vector<ir::Value*>& OutputValues() const { return output_values_; }

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
  ir::Block* FirstBlock() const {
    ASSERT(!blocks_.empty());
    return blocks_.front().get();
  }

  // Owns all the blocks.
  std::vector<std::unique_ptr<ir::Block>> blocks_;

  // Owns all the instructions.
  std::vector<std::unique_ptr<ir::Value>> values_;

  // The output values, one per input expression.
  std::vector<ir::Value*> output_values_;

  // Next available value name, starting at zero.
  uint32_t insertion_point_{0};

  friend struct ir::IRFormVisitor;
};

}  // namespace math
