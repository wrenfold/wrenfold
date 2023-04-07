// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <typeindex>
#include <variant>
#include <vector>

#include "expressions/function_expressions.h"  //temporary

namespace math::ast {

class ScalarType {
 public:
  std::size_t Dimension() const { return 1; }

  std::string ToString() const;
};

class MatrixType {
 public:
  MatrixType(index_t rows, index_t cols) : rows_(rows), cols_(cols) {}

  std::size_t Dimension() const {
    return static_cast<std::size_t>(rows_) * static_cast<std::size_t>(cols_);
  }

  index_t NumRows() const { return rows_; }
  index_t NumCols() const { return cols_; }

  bool HasUnitDimension() const { return rows_ == 1 || cols_ == 1; }

  // Convert to [row, col] indices (assuming row major order).
  std::pair<index_t, index_t> ComputeIndices(std::size_t element) const {
    ASSERT_LESS(element, Dimension());
    return std::make_pair(static_cast<index_t>(element) / cols_,
                          static_cast<index_t>(element) % cols_);
  }

  std::string ToString() const;

 private:
  index_t rows_;
  index_t cols_;
};

// TODO: Add ability to add custom type.
using Type = std::variant<ScalarType, MatrixType>;

inline std::size_t Dimension(const Type& type) {
  return std::visit([](const auto& type) { return type.Dimension(); }, type);
}

// Store an argument to a function.
class Argument {
 public:
  using shared_ptr = std::shared_ptr<const Argument>;

  Argument(const std::string_view& name, ast::Type type, bool is_optional)
      : name_(name), type_(std::move(type)), is_optional_(is_optional) {}

  const std::string& Name() const { return name_; }

  // Get the dimension of the underlying type.
  std::size_t TypeDimension() const { return ::math::ast::Dimension(type_); }

  const ast::Type& Type() const { return type_; }

  // Is this argument optional? Presently only output arguments may be optional.
  bool IsOptional() const { return is_optional_; }

 private:
  std::string name_;
  ast::Type type_;
  bool is_optional_;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
struct FunctionSignature {
 public:
  explicit FunctionSignature(std::string name) : function_name(std::move(name)) {}

  template <typename... Args>
  void AddInput(Args&&... args) {
    input_args.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  template <typename... Args>
  void AddOutput(Args&&... args) {
    output_args.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  void AddReturnValue(ast::Type type) { return_values.push_back(std::move(type)); }

  std::string function_name;
  std::vector<std::shared_ptr<const ast::Argument>> input_args{};
  std::vector<std::shared_ptr<const ast::Argument>> output_args{};
  std::vector<ast::Type> return_values{};
};

// clang-format off
using Variant = std::variant<
    struct Add,
//    struct ArrayAccess,
    struct Assignment,
    struct Call,
//    struct Cast,
    struct Conditional,
    struct ConstructMatrix,
    struct Declaration,
    struct FloatConstant,
    struct InputValue,
    struct IntegerConstant,
    struct Multiply,
    struct OutputExists,
    struct ReturnValue,
    struct VariableRef
    >;
// clang-format on

// This is a shared_ptr so that we can copy AST members.
// Copying is desirable to satisfy the pybind11 wrapper.
using VariantPtr = std::shared_ptr<const Variant>;

// Create shared-ptr to variant and fill it with type `T`.
template <typename T, typename... Args>
VariantPtr MakeVariantPtr(Args&&... args) {
  return std::make_shared<const Variant>(T{std::forward<Args>(args)...});
}

// Usage of a variable.
struct VariableRef {
  // Name of the variable. TODO: Small string.
  std::string name;

  explicit VariableRef(std::string name) : name(std::move(name)) {}

  std::string ToString() const;
};

struct Add {
  VariantPtr left;
  VariantPtr right;

  Add(VariantPtr left, VariantPtr right) : left(std::move(left)), right(std::move(right)) {}

  std::string ToString() const;
};

struct ArrayAccess {
  VariantPtr arg;
  std::size_t element;
};

struct Assignment {
  std::variant<VariableRef, std::shared_ptr<const Argument>> left;
  VariantPtr right;

  Assignment(std::variant<VariableRef, std::shared_ptr<const Argument>> left, VariantPtr right)
      : left(std::move(left)), right(std::move(right)) {}

  std::string ToString() const;
};

struct Call {
  std::variant<UnaryFunctionName, BinaryFunctionName> function;
  std::vector<Variant> args;

  Call(std::variant<UnaryFunctionName, BinaryFunctionName> function, std::vector<Variant>&& args);

  template <typename... Args>
  explicit Call(std::variant<UnaryFunctionName, BinaryFunctionName> function, Args&&... inputs)
      : function(function) {
    args.reserve(sizeof...(inputs));
    (args.emplace_back(std::forward<Args>(inputs)), ...);
  }

  std::string ToString() const;
};

struct Cast {
  VariantPtr arg;
};

struct Compare {
  enum class ComparisonType {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
  };
  ComparisonType operation{};
  VariantPtr left;
  VariantPtr right;
};

// An if/else statement.
struct Conditional {
  // Condition of the if statement.
  VariantPtr condition;
  // Statements if the condition is true:
  std::vector<Variant> if_branch;
  // Statements if the condition is false:
  std::vector<Variant> else_branch;

  Conditional(VariantPtr condition, std::vector<Variant> if_branch,
              std::vector<Variant> else_branch);

  std::string ToString() const;
};

// ConstructMatrix a type from the provided arguments.
struct ConstructMatrix {
  ast::MatrixType type;
  std::vector<Variant> args;

  ConstructMatrix(ast::MatrixType, std::vector<Variant> args);

  std::string ToString() const;
};

struct Declaration {
  // Name for the value being declared
  std::string name;
  // Type of the value:
  Type type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  VariantPtr value;

  Declaration(std::string name, Type type, VariantPtr value);

  std::string ToString() const;
};

struct FloatConstant {
  double value;

  explicit FloatConstant(double v) : value(v) {}

  std::string ToString() const;
};

// Signature and body of a function.
struct FunctionDefinition {
  FunctionSignature signature;
  std::vector<ast::Variant> body;

  FunctionDefinition(FunctionSignature signature, std::vector<ast::Variant> body)
      : signature(std::move(signature)), body(std::move(body)) {}
};

// Access an input argument at a specific index.
struct InputValue {
  std::shared_ptr<const Argument> argument;
  index_t element;

  std::string ToString() const;
};

struct IntegerConstant {
  std::int64_t value;

  std::string ToString() const;
};

struct Multiply {
  VariantPtr left;
  VariantPtr right;

  Multiply(VariantPtr left, VariantPtr right) : left(std::move(left)), right(std::move(right)) {}

  std::string ToString() const;
};

// Check if an output exists.
struct OutputExists {
  // The argument this output corresponds to.
  std::shared_ptr<const Argument> argument;

  explicit OutputExists(std::shared_ptr<const Argument> arg) : argument(std::move(arg)) {}

  std::string ToString() const;
};

struct ReturnValue {
  // All the return values.
  std::vector<Variant> values{};

  explicit ReturnValue(std::vector<Variant> values);

  std::string ToString() const;
};

// method definitions:

inline ConstructMatrix::ConstructMatrix(ast::MatrixType type, std::vector<Variant> args)
    : type(type), args(std::move(args)) {}

inline Declaration::Declaration(std::string name, Type type, VariantPtr value)
    : name(std::move(name)), type(std::move(type)), value(std::move(value)) {}

inline Call::Call(std::variant<UnaryFunctionName, BinaryFunctionName> function,
                  std::vector<Variant>&& args)
    : function(function), args(std::move(args)) {}

inline Conditional::Conditional(VariantPtr condition, std::vector<Variant> if_branch,
                                std::vector<Variant> else_branch)
    : condition(std::move(condition)),
      if_branch(std::move(if_branch)),
      else_branch(std::move(else_branch)) {}

// inline OutputBlock::OutputBlock(std::shared_ptr<const Argument> arg,
//                                 std::vector<ast::Variant> statements,
//                                 std::vector<VariableRef> outputs)
//     : argument(std::move(arg)), statements(std::move(statements)), outputs(std::move(outputs)) {}

inline ReturnValue::ReturnValue(std::vector<Variant> values) : values(std::move(values)) {}

}  // namespace math::ast
