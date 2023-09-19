// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <typeindex>
#include <variant>
#include <vector>

#include "assertions.h"
#include "enumerations.h"
#include "hashing.h"

namespace math {
struct FlatIr;
struct OutputIr;
}  // namespace math

namespace math::ast {

class ScalarType {
 public:
  explicit ScalarType(NumericType numeric_type) : numeric_type_(numeric_type) {}

  std::size_t Dimension() const { return 1; }

  //  std::string ToString() const;

  NumericType GetNumericType() const { return numeric_type_; }

 private:
  NumericType numeric_type_;
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

enum class ArgumentDirection {
  Input,
  Output,
  OptionalOutput,
};

// Store an argument to a function.
class Argument {
 public:
  using shared_ptr = std::shared_ptr<const Argument>;

  Argument(const std::string_view& name, ast::Type type, ArgumentDirection direction)
      : name_(name), type_(std::move(type)), direction_(direction) {}

  const std::string& Name() const { return name_; }

  // Get the dimension of the underlying type.
  std::size_t TypeDimension() const { return ::math::ast::Dimension(type_); }

  const ast::Type& Type() const { return type_; }

  // Is the argument type a matrix.
  bool IsMatrix() const { return std::holds_alternative<ast::MatrixType>(type_); }

  // Is this argument optional? Presently only output arguments may be optional.
  bool IsOptional() const { return direction_ == ArgumentDirection::OptionalOutput; }

  // Argument direction.
  ArgumentDirection Direction() const { return direction_; }

 private:
  std::string name_;
  ast::Type type_;
  ArgumentDirection direction_;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
struct FunctionSignature {
 public:
  explicit FunctionSignature(std::string name) : function_name(std::move(name)) {}

  template <typename... Args>
  void AddArgument(Args&&... args) {
    arguments.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  // Find an argument by name.
  const std::shared_ptr<const ast::Argument>& GetArgument(const std::string_view str) const {
    auto it = std::find_if(arguments.begin(), arguments.end(),
                           [&](const auto& arg) { return arg->Name() == str; });
    ASSERT(it != arguments.end(), "Argument does not exist: {}", str);
    return *it;
  }

  std::string function_name;
  std::vector<std::shared_ptr<const ast::Argument>> arguments{};
  std::optional<ast::Type> return_value;
};

// clang-format off
using Variant = std::variant<
    struct Add,
    struct AssignTemporary,
    struct AssignOutputArgument,
    struct Branch,
    struct Call,
    struct Cast,
    struct Compare,
    struct ConstructReturnValue,
    struct Declaration,
    struct FloatConstant,
    struct InputValue,
    struct IntegerConstant,
    struct Multiply,
    struct OutputExists,
    struct VariableRef
    >;
// clang-format on

// This is a shared_ptr so that we can copy AST members.
// Copying is desirable to satisfy the pybind11 wrapper.
using VariantPtr = std::shared_ptr<const Variant>;

// Usage of a variable.
struct VariableRef {
  // Name of the variable.
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

struct AssignTemporary {
  std::string left;
  VariantPtr right;

  AssignTemporary(std::string left, VariantPtr right)
      : left(std::move(left)), right(std::move(right)) {}

  template <typename T, typename = std::enable_if_t<std::is_constructible_v<ast::Variant, T>>>
  AssignTemporary(std::string left, T&& arg)
      : left(left), right(std::make_shared<const ast::Variant>(std::forward<T>(arg))) {}

  std::string ToString() const;
};

// Assign values to an output argument. All output values are written in one operation.
struct AssignOutputArgument {
  std::shared_ptr<const Argument> argument;
  std::vector<Variant> values;
};

// An if/else statement.
struct Branch {
  // Condition of the if statement.
  VariantPtr condition;
  // Statements if the condition is true:
  std::vector<Variant> if_branch;
  // Statements if the condition is false:
  std::vector<Variant> else_branch;

  Branch(VariantPtr condition, std::vector<Variant>&& if_branch,
         std::vector<Variant>&& else_branch);

  template <typename T, typename = std::enable_if_t<std::is_constructible_v<ast::Variant, T>>>
  Branch(T&& arg, std::vector<Variant>&& if_branch, std::vector<Variant>&& else_branch)
      : condition{std::make_shared<const ast::Variant>(std::forward<T>(arg))},
        if_branch(std::move(if_branch)),
        else_branch(std::move(else_branch)) {}

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
  NumericType destination_type;
  VariantPtr arg;

  Cast(NumericType destination_type, const VariantPtr& arg)
      : destination_type(destination_type), arg(arg) {}
};

struct Compare {
  RelationalOperation operation{};
  VariantPtr left;
  VariantPtr right;
};

// Construct a type from the provided arguments.
struct ConstructReturnValue {
  ast::Type type;
  std::vector<Variant> args;

  ConstructReturnValue(ast::Type, std::vector<Variant>&& args);

  std::string ToString() const;
};

struct Declaration {
  // Name for the value being declared
  std::string name;
  // Type of the value:
  Type type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  VariantPtr value{};

  Declaration(std::string name, Type type, VariantPtr value);

  // Construct w/ no rhs.
  Declaration(std::string name, Type type) : name(std::move(name)), type(type) {}

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

  FunctionDefinition(FunctionSignature signature, std::vector<ast::Variant> body);
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

// method definitions:

inline FunctionDefinition::FunctionDefinition(FunctionSignature signature,
                                              std::vector<ast::Variant> body)
    : signature(std::move(signature)), body(std::move(body)) {}

inline ConstructReturnValue::ConstructReturnValue(ast::Type type, std::vector<Variant>&& args)
    : type(type), args(std::move(args)) {}

inline Declaration::Declaration(std::string name, Type type, VariantPtr value)
    : name(std::move(name)), type(std::move(type)), value(std::move(value)) {}

inline Call::Call(std::variant<UnaryFunctionName, BinaryFunctionName> function,
                  std::vector<Variant>&& args)
    : function(function), args(std::move(args)) {}

inline Branch::Branch(VariantPtr condition, std::vector<Variant>&& if_branch,
                      std::vector<Variant>&& else_branch)
    : condition(std::move(condition)),
      if_branch(std::move(if_branch)),
      else_branch(std::move(else_branch)) {}

// Create AST from the IR:
ast::FunctionDefinition CreateAST(const math::OutputIr& ir, const FunctionSignature& signature);

}  // namespace math::ast
