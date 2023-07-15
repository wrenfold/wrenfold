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

  // Run the provided callable object on any successor blocks that this block jumps to.
  template <typename Callable>
  void VisitSuccessors(Callable&& callable) const;

  void AddAncestor(BlockPtr b);

  void RemoveAncestor(BlockPtr b);
};

// Determine the underlying numeric type of the provided value.
NumericType DetermineValueType(ValuePtr v);

struct Add {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "add"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Add&) const { return true; }

  const NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

// Evaluates to true if the specified output index is required.
struct OutputRequired {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "oreq"; }
  constexpr std::size_t Hash() const { return arg_position; }
  constexpr bool IsSame(const OutputRequired& other) const {
    return arg_position == other.arg_position;
  }

  constexpr NumericType DetermineType() const { return NumericType::Bool; }

  explicit OutputRequired(std::size_t arg_position) : arg_position(arg_position) {}

  std::size_t arg_position;
};

struct Mul {
  constexpr static bool IsCommutative() { return true; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "mul"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Mul&) const { return true; }

  const NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

struct Pow {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 2; }
  constexpr std::string_view ToString() const { return "pow"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Pow&) const { return true; }

  const NumericType DetermineType(ValuePtr a, ValuePtr b) const {
    return std::max(std::max(DetermineValueType(a), DetermineValueType(b)), NumericType::Integer);
  }
};

struct Copy {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "copy"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Copy&) const { return true; }

  const NumericType DetermineType(ValuePtr arg) const { return DetermineValueType(arg); }
};

struct Cast {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "cast"; }

  constexpr std::size_t Hash() const { return static_cast<std::size_t>(destination_type); }

  constexpr bool IsSame(const Cast& other) const {
    return destination_type == other.destination_type;
  }

  constexpr NumericType DetermineType(const ValuePtr&) const { return destination_type; }

  constexpr explicit Cast(NumericType destination) : destination_type(destination) {}

  NumericType destination_type;
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return math::ToString(name); }
  constexpr std::size_t Hash() const { return static_cast<std::size_t>(name); }
  constexpr bool IsSame(const CallUnaryFunc& other) const { return name == other.name; }
  constexpr NumericType DetermineType(const ValuePtr&) const { return NumericType::Real; }

  CallUnaryFunc(UnaryFunctionName name) : name(name) {}

  UnaryFunctionName name;
};

struct Cond {
  constexpr static int NumValueOperands() { return 3; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "cond"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Cond&) const { return true; }

  NumericType DetermineType(const ValuePtr&, const ValuePtr& true_val,
                            const ValuePtr& false_val) const {
    return std::max(DetermineValueType(true_val), DetermineValueType(false_val));
  }
};

struct Phi {
  constexpr static int NumValueOperands() { return 2; }
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "phi"; }
  constexpr std::size_t Hash() const { return 0; }
  constexpr bool IsSame(const Phi&) const { return true; }

  NumericType DetermineType(const ValuePtr& a, const ValuePtr&) const {
    // Both values should be the same:
    return DetermineValueType(a);
  }
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
  constexpr NumericType DetermineType(const ValuePtr&, const ValuePtr&) const {
    return NumericType::Bool;
  }

  explicit Compare(const RelationalOperation operation) : operation(operation) {}

  RelationalOperation operation;
};

struct Load {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 0; }
  constexpr std::string_view ToString() const { return "load"; }
  std::size_t Hash() const { return HashExpression(expr); }
  bool IsSame(const Load& other) const { return expr.IsIdenticalTo(other.expr); }

  // Defined in cc file.
  NumericType DetermineType() const;

  explicit Load(Expr expr) : expr(std::move(expr)) {}

  Expr expr;
};

struct Save {
  constexpr static bool IsCommutative() { return false; }
  constexpr static int NumValueOperands() { return 1; }
  constexpr std::string_view ToString() const { return "save"; }

  constexpr std::size_t Hash() const { return HashCombine(OutputKeyHasher{}(key), output_index); }

  constexpr bool IsSame(const Save& other) const {
    return key == other.key && output_index == other.output_index;
  }

  // Allow `Save` to be ordered by key, then output index.
  bool operator<(const Save& other) const {
    return std::make_pair(key, output_index) < std::make_pair(other.key, other.output_index);
  }

  OutputKey key;
  std::size_t output_index;
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
using Operation = std::variant<Add, Mul, Pow, CallUnaryFunc, Cast, Compare, Cond, Copy, Phi, Load,
                               Save, OutputRequired, Jump, ConditionalJump>;

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
      : name_(name),
        parent_(parent),
        op_(std::forward<OpType>(operation)),
        operands_(std::move(operands)) {
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
      : Value(name, parent, std::forward<OpType>(operation), MakeValueVector(args...)) {}

  // Access underlying integer.
  constexpr uint32_t Name() const { return name_; }

  // Get the parent block.
  ir::BlockPtr Parent() const { return parent_; }

  // Set the parent pointer.
  void SetParent(ir::BlockPtr b) {
    std::visit(
        [&](const auto& op) {
          using T = std::decay_t<decltype(op)>;
          if constexpr (std::is_same_v<T, ir::Jump>) {
            op.Next()->RemoveAncestor(parent_);
            op.Next()->AddAncestor(b);
          } else if constexpr (std::is_same_v<T, ir::ConditionalJump>) {
            op.NextTrue()->RemoveAncestor(parent_);
            op.NextTrue()->AddAncestor(b);
            op.NextFalse()->RemoveAncestor(parent_);
            op.NextFalse()->AddAncestor(b);
          }
        },
        op_);
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
    return std::any_of(consumers_.begin(), consumers_.end(),
                       [](const ValuePtr& v) { return v->IsPhi(); });
  }

  // Replace an operand to this instruction with another.
  void ReplaceOperand(const ValuePtr old, const ValuePtr replacement) {
    const ValuePtr self{this};
    for (ir::ValuePtr& operand : operands_) {
      if (operand == old) {
        // Note we don't remove the consumer from `operand`, that happens in ReplaceWith(...)
        operand = replacement;
        replacement->AddConsumer(self);
      }
    }
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
    op_ = std::forward<OpType>(op);
    NotifyOperands();
    if constexpr (OpType::IsCommutative()) {
      SortOperands();
    }
    CheckNumOperands<OpType>();
  }

  void AddConsumer(const ir::ValuePtr v) {
    // The value might be consumed twice by the same expression, for instance: pow(x, x)
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it == consumers_.end()) {
      consumers_.push_back(v);
    }
  }

  void RemoveConsumer(ir::Value* v) {
    auto it = std::find(consumers_.begin(), consumers_.end(), v);
    if (it != consumers_.end()) {
      // Might have already been removed, if we were an operand twice.
      consumers_.erase(it);
    }
  }

  // Access instruction operands.
  const std::vector<ValuePtr>& Operands() const { return operands_; }

  // Number of operands.
  std::size_t NumOperands() const { return operands_.size(); }

  // Get the first operand.
  const ValuePtr& Front() const { return operands_.front(); }

  // Access i'th operand:
  ValuePtr operator[](std::size_t i) const { return operands_[i]; }

  // Access all consumers of this value.
  const std::vector<ValuePtr>& Consumers() const { return consumers_; }

  // Number of values that directly consume this one.
  std::size_t NumConsumers() const { return consumers_.size(); }

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

  template <typename Predicate>
  bool AllOperandsSatisfy(Predicate&& predicate) const {
    return std::all_of(operands_.begin(), operands_.end(), std::forward<Predicate>(predicate));
  }

  // Replace this value w/ the argument.
  // All downstream consumers have their arguments swapped, and the consumer list is cleared.
  void ReplaceWith(ValuePtr other);

  // Nuke this value. Only valid if it is not consumed.
  void Remove();

  // True if there are no consumers of this value.
  bool IsUnused() const { return !IsJump() && consumers_.empty(); }

  // Determine the numeric type of the resulting expression.
  NumericType DetermineType() const {
    return std::visit(
        [&](const auto& op) -> NumericType {
          using T = std::decay_t<decltype(op)>;
          using OmittedTypes = TypeList<Save, Jump, ConditionalJump>;
          if constexpr (ContainsType<T, OmittedTypes>) {
            throw TypeError("Operation does not have a numeric type");
          } else if constexpr (T::NumValueOperands() == 0) {
            return op.DetermineType();
          } else if constexpr (T::NumValueOperands() == 1) {
            return op.DetermineType(operands_[0]);
          } else if constexpr (T::NumValueOperands() == 2) {
            return op.DetermineType(operands_[0], operands_[1]);
          } else if constexpr (T::NumValueOperands() == 3) {
            return op.DetermineType(operands_[0], operands_[1], operands_[2]);
          } else {
            return op.DetermineType(operands_);
          }
        },
        op_);
  }

 protected:
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
    const ValuePtr self{this};
    for (const ValuePtr& operand : operands_) {
      operand->AddConsumer(self);
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

// Inline method definition:
inline NumericType DetermineValueType(ValuePtr v) { return v->DetermineType(); }

}  // namespace ir

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

  friend struct ir::IRFormVisitor;
};

}  // namespace math
