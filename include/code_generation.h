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

// Operands are either the result of an assignment (Value), or an input value
// (in which case we just use Expr).
using Operand = std::variant<Value, Expr>;

// Struct that orders the `Operand` type.
struct OperandOrder {
  // Order assignments by index.
  RelativeOrder operator()(Value l, Value r) const {
    if (l < r) {
      return RelativeOrder::LessThan;
    } else if (l == r) {
      return RelativeOrder::Equal;
    } else {
      return RelativeOrder::GreaterThan;
    }
  }

  // Order assignment operands before input value operands.
  constexpr RelativeOrder operator()(const Value&, const Expr&) const {
    return RelativeOrder::LessThan;
  }
  constexpr RelativeOrder operator()(const Expr&, const Value&) const {
    return RelativeOrder::GreaterThan;
  }

  // Order input value operands as we typically would.
  RelativeOrder operator()(const Expr& l, const Expr& r) const { return ExpressionOrder(l, r); }

  RelativeOrder operator()(const Operand& l, const Operand& r) const {
    return std::visit(*this, l, r);
  }
};

struct OperandOrderBool {
  bool operator()(const Operand& l, const Operand& r) const {
    return OperandOrder{}(l, r) == RelativeOrder::LessThan;
  }
};

template <typename Derived, std::size_t N>
struct OperationBase {
  // Forward everything into the `args` array.
  template <typename... Ts>
  explicit OperationBase(Ts&&... inputs) : args{Operand(std::forward<Ts>(inputs))...} {
    if constexpr (IsCommutative()) {
      std::sort(args.begin(), args.end(), OperandOrderBool{});
    }
  }

  constexpr static bool IsCommutative() { return Derived::IsCommutative(); }

  // Check if two operations have identical arguments.
  bool operator==(const OperationBase& other) const {
    return std::equal(args.begin(), args.end(), other.args.begin(), other.args.end(),
                      [](const Operand& a, const Operand& b) {
                        return OperandOrder{}(a, b) == RelativeOrder::Equal;
                      });
  }

  std::array<Operand, N> args{};
};

struct Add : public OperationBase<Add, 2> {
  constexpr static bool IsCommutative() { return true; }
  constexpr std::string_view ToString() const { return "add"; }
  Add(Operand left, Operand right) : OperationBase(std::move(left), std::move(right)) {}
};

struct Mul : public OperationBase<Mul, 2> {
  constexpr static bool IsCommutative() { return true; }
  constexpr std::string_view ToString() const { return "mul"; }
  Mul(Operand left, Operand right) : OperationBase(std::move(left), std::move(right)) {}
};

struct Pow : public OperationBase<Pow, 2> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "pow"; }
  Pow(Operand base, Operand exponent) : OperationBase(std::move(base), std::move(exponent)) {}
};

struct Load : public OperationBase<Load, 1> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "load"; }
  explicit Load(Expr input) : OperationBase(std::move(input)) {}
  explicit Load(Value input) : OperationBase(input) {}
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc : public OperationBase<CallUnaryFunc, 1> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return math::ToString(name); }
  CallUnaryFunc(UnaryFunctionName name, Operand arg) : OperationBase(std::move(arg)), name(name) {}
  UnaryFunctionName name;

  bool operator==(const CallUnaryFunc& other) const {
    return name == other.name && OperationBase::operator==(other);
  }
};

struct Cond : public OperationBase<Cond, 3> {
  constexpr static bool IsCommutative() { return false; }
  constexpr std::string_view ToString() const { return "cond"; }
  explicit Cond(Operand cond, Operand if_branch, Operand else_branch)
      : OperationBase(std::move(cond), std::move(if_branch), std::move(else_branch)) {}
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

  explicit Compare(const RelationalOperation operation, Operand left, Operand right)
      : OperationBase(std::move(left), std::move(right)), operation(operation) {}

  bool operator==(const Compare& other) const {
    return operation == other.operation && OperationBase::operator==(other);
  }

  RelationalOperation operation;
};

// Different operations are represented by a variant.
using Operation = std::variant<Add, Mul, Pow, Load, CallUnaryFunc, Compare, Cond>;

// Pair together a target value and an operation.
struct OpWithTarget {
  Value target;
  Operation op;

  OpWithTarget(const ir::Value target, ir::Operation&& op) : target(target), op(std::move(op)) {}
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

  // Number of operations:
  std::size_t NumOperations() const { return operations_.size(); }

 protected:
  // Insert a new operand of type `T` w/ the provided args.
  template <typename T, typename... Args>
  ir::Operand PushOperation(Args&&... args);

  // Propagate copied operands for the provided operation, which is modified in place.
  void PropagateCopiedOperands(
      ir::Operation& operation,
      const std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash>& op_for_value) const;

  // Eliminate any unused values from `operations_`.
  void StripUnusedValues(
      const std::unordered_map<ir::Value, const ir::Operation*, ir::ValueHash>& op_for_value);

  // An array of operations and the value IDs they compute.
  std::vector<ir::OpWithTarget> operations_;

  // The output values, one per input expression.
  std::vector<ir::Value> output_values_;

  // Next available value, starting at zero.
  ir::Value insertion_point_{0};

  friend struct ir::IRFormVisitor;
};

}  // namespace math
