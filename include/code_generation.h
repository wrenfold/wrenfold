// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"

#include <typeindex>
#include <unordered_map>
#include <variant>
#include <vector>

#include "ast.h"
#include "expressions/function_expressions.h"  //  temporary
#include "function_evaluator.h"
#include "ordering.h"

namespace math {
namespace ir {

// Just a wrapper around an integer, to add some clarity via types.
struct Value {
  // Construct from integer.
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
  OrderVisitor::RelativeOrder operator()(Value l, Value r) const {
    if (l < r) {
      return OrderVisitor::RelativeOrder::LessThan;
    } else if (l == r) {
      return OrderVisitor::RelativeOrder::Equal;
    } else {
      return OrderVisitor::RelativeOrder::GreaterThan;
    }
  }

  // Order assignment operands before input value operands.
  constexpr OrderVisitor::RelativeOrder operator()(const Value&, const Expr&) const {
    return OrderVisitor::RelativeOrder::LessThan;
  }
  constexpr OrderVisitor::RelativeOrder operator()(const Expr&, const Value&) const {
    return OrderVisitor::RelativeOrder::GreaterThan;
  }

  // Order input value operands as we typically would.
  OrderVisitor::RelativeOrder operator()(const Expr& l, const Expr& r) const {
    return VisitBinaryStruct(l, r, OrderVisitor{});
  }

  OrderVisitor::RelativeOrder operator()(const Operand& l, const Operand& r) const {
    return std::visit(*this, l, r);
  }
};

struct OperandOrderBool {
  bool operator()(const Operand& l, const Operand& r) const {
    return OperandOrder{}(l, r) == OrderVisitor::RelativeOrder::LessThan;
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
                        return OperandOrder{}(a, b) == OrderVisitor::RelativeOrder::Equal;
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

// Different operations are represented by a variant.
using Operation = std::variant<Add, Mul, Pow, Load, CallUnaryFunc>;

}  // namespace ir

struct FunctionDescription;

// Object for creating the intermediate representation. The IR is then given to the code-generator
// to be simplified.
struct IrBuilder {
 public:
  // Construct from a set of output expressions.
  explicit IrBuilder(const std::vector<Expr>& expressions);

  // Get the values indices for the outputs.
  const std::vector<ir::Value>& OutputValues() const { return output_values_; }

  // For the specified value, format the list of IR operations required to make it.
  std::string FormatIR(const ir::Value& value) const;

  // Format the IR for the given output index.
  std::string FormatIRForOutput(std::size_t index) const {
    ASSERT_LESS(index, output_values_.size());
    return FormatIR(output_values_[index]);
  }

  // Format IR for every value.
  std::string FormatIRAllOutputs() const;

  // Size of value numbers when printed (# digits).
  std::size_t ValuePrintWidth() const;

  // Recreate the expression tree for the specified IR value.
  Expr CreateExpression(const ir::Value& value) const;

  // Recreate the expression tree for the specified output (by index).
  Expr CreateExpressionForOutput(std::size_t index) const {
    ASSERT_LESS(index, output_values_.size());
    return CreateExpression(output_values_[index]);
  }

  // Get the traversal order for all outputs:
  std::vector<ir::Value> GetTraversalOrder() const;

  // Create AST to be emitted.
  std::vector<ast::Variant> CreateAST(const FunctionDescription& description);

  // Eliminate duplicated operations.
  void EliminateDuplicates();

  // Number of operations:
  std::size_t NumOperations() const { return operations_.size(); }

  // Find an operation from its value:
  const ir::Operation& Find(const ir::Value& val) const { return operations_.at(val); }

 protected:
  // Insert a new operand of type `T` w/ the provided args.
  template <typename T, typename... Args>
  ir::Operand PushOperation(Args&&... args);

  // Get depth-first traversal order of operation tree.
  std::vector<ir::Value> GetTraversalOrder(const ir::Value& value) const;

  // Propagate copied operands for the provided operation, which is modified in place.
  void PropagateCopiedOperands(ir::Operation& operation) const;

  // Eliminate any unused values from `operations_`.
  void StripUnusedValues();

  // Map of value id to an operation.
  std::unordered_map<ir::Value, ir::Operation, ir::ValueHash> operations_;

  // The output values, one per input expression.
  std::vector<ir::Value> output_values_;

  // Next available value, starting at zero.
  ir::Value insertion_point_{0};

  friend struct ir::IRFormVisitor;
};

// Fwd declare.
class CodeFormatter;

class CodeGeneratorBase {
 public:
  virtual ~CodeGeneratorBase() = default;

  //
  std::string Generate(const FunctionDescription& func, const std::vector<ast::Variant>& ast);

 protected:
  virtual void GenerateImpl(CodeFormatter& formatter, const FunctionDescription& func,
                            const std::vector<ast::Variant>& ast) = 0;
};

template <typename Generator>
struct CodeGeneratorImpl : public CodeGeneratorBase {
 public:
  // Construct w/ user provided implementation.
  explicit CodeGeneratorImpl(Generator&& impl) : impl_(std::move(impl)) {}

 protected:
  void GenerateImpl(CodeFormatter& formatter, const FunctionDescription& func,
                    const std::vector<ast::Variant>& ast) override final {
    impl_.Generate(formatter, func, ast);
  }

 private:
  Generator impl_;
};

}  // namespace math