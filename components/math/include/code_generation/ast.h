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

  constexpr NumericType numeric_type() const { return numeric_type_; }

 private:
  NumericType numeric_type_;
};

class MatrixType {
 public:
  MatrixType(index_t rows, index_t cols) : rows_(rows), cols_(cols) {}

  constexpr index_t rows() const { return rows_; }
  constexpr index_t cols() const { return cols_; }

  // Convert to [row, col] indices (assuming row major order).
  std::pair<index_t, index_t> compute_indices(std::size_t element) const {
    ASSERT_LESS(element, static_cast<std::size_t>(rows_) * static_cast<std::size_t>(cols_));
    return std::make_pair(static_cast<index_t>(element) / cols_,
                          static_cast<index_t>(element) % cols_);
  }

 private:
  index_t rows_;
  index_t cols_;
};

// TODO: Add ability to add custom type.
using Type = std::variant<ScalarType, MatrixType>;

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

  // Name of the argument.
  const std::string& name() const { return name_; }

  // Type of the argument.
  const ast::Type& type() const { return type_; }

  // Is the argument type a matrix.
  constexpr bool is_matrix() const { return std::holds_alternative<ast::MatrixType>(type_); }

  // Is this argument optional? Presently only output arguments may be optional.
  bool is_optional() const { return direction_ == ArgumentDirection::OptionalOutput; }

  // Argument direction.
  ArgumentDirection direction() const { return direction_; }

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
  void add_argument(Args&&... args) {
    arguments.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  // Find an argument by name.
  const std::shared_ptr<const ast::Argument>& get_argument(const std::string_view str) const {
    auto it = std::find_if(arguments.begin(), arguments.end(),
                           [&](const auto& arg) { return arg->name() == str; });
    ASSERT(it != arguments.end(), "Argument does not exist: {}", str);
    return *it;
  }

  // Are any of the arguments to this function a matrix?
  bool has_matrix_arguments() const {
    return std::any_of(arguments.begin(), arguments.end(),
                       [](const auto& arg) { return arg->is_matrix(); });
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
    struct OptionalOutputBranch,
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
};

struct Add {
  VariantPtr left;
  VariantPtr right;

  Add(VariantPtr left, VariantPtr right) : left(std::move(left)), right(std::move(right)) {}
};

struct AssignTemporary {
  std::string left;
  VariantPtr right;

  AssignTemporary(std::string left, VariantPtr right)
      : left(std::move(left)), right(std::move(right)) {}

  template <typename T, typename = std::enable_if_t<std::is_constructible_v<ast::Variant, T>>>
  AssignTemporary(std::string left, T&& arg)
      : left(left), right(std::make_shared<const ast::Variant>(std::forward<T>(arg))) {}
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
};

struct Call {
  BuiltInFunctionName function;
  std::vector<Variant> args;

  template <typename... Args>
  explicit Call(BuiltInFunctionName function, Args&&... inputs)
      : function(function), args{std::forward<Args>(inputs)...} {}
};

struct Cast {
  NumericType destination_type;
  NumericType source_type;
  VariantPtr arg;

  Cast(NumericType destination_type, NumericType source_type, const VariantPtr& arg)
      : destination_type(destination_type), source_type(source_type), arg(arg) {}
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
};

struct Declaration {
  // Name for the value being declared
  std::string name;
  // Type of the value:
  NumericType type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  VariantPtr value{};

  Declaration(std::string name, NumericType type, VariantPtr value);

  // Construct w/ no rhs.
  Declaration(std::string name, NumericType type) : name(std::move(name)), type(type) {}
};

struct FloatConstant {
  double value;

  explicit FloatConstant(double v) : value(v) {}
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
};

struct IntegerConstant {
  std::int64_t value;
};

struct Multiply {
  VariantPtr left;
  VariantPtr right;

  Multiply(VariantPtr left, VariantPtr right) : left(std::move(left)), right(std::move(right)) {}
};

// A one-sided branch that assigns to an optional output, after checking for its existence.
// This corresponds to a block that looks something like:
//  if (<argument exists>) {
//    ... statements ...
//  }
struct OptionalOutputBranch {
  // The argument this output corresponds to.
  std::shared_ptr<const Argument> argument;

  // Statements in the if-branch.
  std::vector<Variant> statements;

  explicit OptionalOutputBranch(std::shared_ptr<const Argument> arg,
                                std::vector<Variant>&& statements)
      : argument(std::move(arg)), statements(std::move(statements)) {}
};

// method definitions:

inline FunctionDefinition::FunctionDefinition(FunctionSignature signature,
                                              std::vector<ast::Variant> body)
    : signature(std::move(signature)), body(std::move(body)) {}

inline ConstructReturnValue::ConstructReturnValue(ast::Type type, std::vector<Variant>&& args)
    : type(type), args(std::move(args)) {}

inline Declaration::Declaration(std::string name, NumericType type, VariantPtr value)
    : name(std::move(name)), type(type), value(std::move(value)) {}

inline Branch::Branch(VariantPtr condition, std::vector<Variant>&& if_branch,
                      std::vector<Variant>&& else_branch)
    : condition(std::move(condition)),
      if_branch(std::move(if_branch)),
      else_branch(std::move(else_branch)) {}

// Create AST from the IR:
std::vector<ast::Variant> create_ast(const math::OutputIr& ir, const FunctionSignature& signature);

}  // namespace math::ast
