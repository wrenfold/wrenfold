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

namespace math {
namespace ir {

// Just a wrapper around an integer, to add some clarity via types.
struct Value {
  // ConstructMatrix from integer.
  explicit Value(uint32_t index) : v_(index) {}

  // Equal if underlying int is the same.
  constexpr bool operator==(const Value& other) const { return v_ == other.v_; }

  // Order by index.
  constexpr bool operator<(const Value& other) const { return v_ < other.v_; }

  // Hash is just std::hash
  std::size_t Hash() const { return std::hash<std::size_t>{}(v_); }

  // Access underlying integer.
  constexpr uint32_t Id() const { return v_; }

  // Increment ID.
  void Increment() { v_++; }

 private:
  uint32_t v_;
};

// Hash operator for `Value`.
struct ValueHash {
  std::size_t operator()(const Value& v) const { return v.Hash(); }
};

struct IRFormVisitor;  //  Fwd declare.
struct Block;

// Base type for operations that accept `ir::Value` arguments (ie. not loads)
template <typename Derived, std::size_t N>
struct OperationBase {
  // Forward everything into the `args` array.
  template <typename... Ts>
  explicit OperationBase(Ts&&... inputs) : args{std::forward<Ts>(inputs)...} {
    static_assert(sizeof...(Ts) == N);
    if constexpr (IsCommutative()) {
      std::sort(args.begin(), args.end());
    }
  }

  constexpr static bool IsCommutative() { return Derived::IsCommutative(); }

  // Check if two operations have identical arguments.
  bool operator==(const OperationBase& other) const {
    return std::equal(args.begin(), args.end(), other.args.begin(), other.args.end());
  }

  std::array<Value, N> args{};
};

struct Add : public OperationBase<Add, 2> {
  using OperationBase::OperationBase;
  constexpr static bool IsCommutative() { return true; }
  constexpr std::string_view ToString() const { return "add"; }
};

struct Mul : public OperationBase<Mul, 2> {
  using OperationBase::OperationBase;
  constexpr static bool IsCommutative() { return true; }
  constexpr std::string_view ToString() const { return "mul"; }
};

struct Pow : public OperationBase<Pow, 2> {
  using OperationBase::OperationBase;
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "pow"; }
};

struct Copy : public OperationBase<Copy, 1> {
  using OperationBase::OperationBase;
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "copy"; }
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc : public OperationBase<CallUnaryFunc, 1> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return math::ToString(name); }
  CallUnaryFunc(UnaryFunctionName name, ir::Value arg) : OperationBase(arg), name(name) {}
  UnaryFunctionName name;

  bool operator==(const CallUnaryFunc& other) const {
    return name == other.name && OperationBase::operator==(other);
  }
};

struct Cond : public OperationBase<Cond, 3> {
  using OperationBase::OperationBase;
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "cond"; }
};

struct Phi : public OperationBase<Phi, 2> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "phi"; }

  Phi(ir::Value v1, ir::Value v2, ir::Block* jump_block)
      : OperationBase(v1, v2), jump_block(jump_block) {
    ASSERT(jump_block);
  }

  // The blocks that define the input
  ir::Block* jump_block;
};

struct Compare : public OperationBase<Compare, 2> {
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

  explicit Compare(const RelationalOperation operation, ir::Value left, ir::Value right)
      : OperationBase(left, right), operation(operation) {}

  bool operator==(const Compare& other) const {
    return operation == other.operation && OperationBase::operator==(other);
  }

  RelationalOperation operation;
};

struct Load {
  Expr expr;
  explicit Load(Expr expr) : expr(std::move(expr)) {}

  bool operator==(const Load& other) const { return expr.IsIdenticalTo(other.expr); }
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, Mul, Pow, Copy, CallUnaryFunc, Compare, Cond, Phi, Load>;

// Pair together a target value and an operation.
struct OpWithTarget {
  Value target;
  Operation op;

  OpWithTarget(const ir::Value target, ir::Operation&& op) : target(target), op(std::move(op)) {}
};

struct Block;

struct Jump {
  Block* next;
};

struct ConditionalJump {
  ir::Value condition;
  Block* next_true;
  Block* next_false;
};

// A block of operations:
struct Block {
  // Unique number to refer to the block (a counter).
  std::size_t name;

  // All the operations in the block, in order.
  std::vector<ir::OpWithTarget> operations;

  // How the block exits.
  std::variant<std::monostate, Jump, ConditionalJump> jump{std::monostate()};

  // Ancestor blocks (blocks that preceded this one).
  std::vector<ir::Block*> ancestors;

  // True if there is no jump statement.
  bool IsEnd() const { return std::holds_alternative<std::monostate>(jump); }

  // Terminate the block with a jump to another block.
  void SetJump(ir::Block* const next) {
    ASSERT(next);
    jump = Jump{next};
    next->ancestors.push_back(this);
  }

  // Terminate the block with a conditional jump to two blocks.
  void SetConditionalJump(ir::Value cond, ir::Block* const next_true, ir::Block* const next_false) {
    ASSERT(next_true);
    ASSERT(next_false);
    jump = ConditionalJump{cond, next_true, next_false};
    next_true->ancestors.push_back(this);
    next_false->ancestors.push_back(this);
  }

  // True if the block has no operations.
  bool IsEmpty() const { return operations.empty(); }
};

}  // namespace ir

// Object for creating the intermediate representation. The IR is then given to the code-generator
// to be simplified.
struct IrBuilder {
 public:
  // ConstructMatrix from a set of output expressions.
  explicit IrBuilder(const std::vector<Expr>& expressions);

  // Get the values indices for the outputs.
  const std::vector<ir::Value>& OutputValues() const { return output_values_; }

  // Format IR for every value.
  std::string ToString() const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  //
  void MergeBlocks();

  // Recreate the expression tree for the specified IR value.
  Expr CreateExpression(const ir::Value& value) const;

  // Recreate the expression tree for the specified output (by index).
  Expr CreateExpressionForOutput(std::size_t index) const {
    ASSERT_LESS(index, output_values_.size());
    return CreateExpression(output_values_[index]);
  }

  // Create AST to be emitted.
  std::vector<ast::Variant> CreateAST(const ast::FunctionSignature& description);

  // Eliminate duplicated operations.
  void EliminateDuplicates();
  void StripUnusedValues();
  void ThreadJumps();

  // Number of operations:
  std::size_t NumOperations() const;

  // Number of conditional jumps.
  std::size_t NumJumps() const;

 protected:
  ir::Block* FirstBlock() const {
    ASSERT(!blocks_.empty());
    return blocks_.front().get();
  }

  // An array of operations and the value IDs they compute.
  std::vector<std::unique_ptr<ir::Block>> blocks_;

  // todo: Delete me
  std::vector<ir::OpWithTarget> operations_;

  // The output values, one per input expression.
  std::vector<ir::Value> output_values_;

  // Next available value, starting at zero.
  ir::Value insertion_point_{0};

  friend struct ir::IRFormVisitor;
};

}  // namespace math
