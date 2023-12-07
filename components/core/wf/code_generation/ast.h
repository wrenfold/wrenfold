// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/code_generation/function_description.h"
#include "wf/enumerations.h"
#include "wf/hashing.h"

namespace wf {
class flat_ir;
class output_ir;
}  // namespace wf

namespace wf::ast {

// clang-format off
using variant = std::variant<
    struct add,
    struct assign_temporary,
    struct assign_output_argument,
    struct branch,
    struct call,
    struct cast,
    struct compare,
    struct construct_return_value,
    struct declaration,
    struct divide,
    struct float_literal,
    struct input_value,
    struct integer_literal,
    struct multiply,
    struct optional_output_branch,
    struct special_constant,
    struct variable_ref
    >;
// clang-format on

// This is a shared_ptr so that we can copy AST members.
// Copying is desirable to satisfy the pybind11 wrapper.
using variant_ptr = std::shared_ptr<const variant>;

// Usage of a variable.
struct variable_ref {
  // Name of the variable.
  std::string name;

  explicit variable_ref(std::string name) : name(std::move(name)) {}
};

// Add two values together.
struct add {
  variant_ptr left;
  variant_ptr right;

  add(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// Assign a value to a temporary variable.
struct assign_temporary {
  std::string left;
  variant_ptr right;

  assign_temporary(std::string left, variant_ptr right)
      : left(std::move(left)), right(std::move(right)) {}

  template <typename T, typename = std::enable_if_t<std::is_constructible_v<ast::variant, T>>>
  assign_temporary(std::string left, T&& arg)
      : left(left), right(std::make_shared<const ast::variant>(std::forward<T>(arg))) {}
};

// Assign values to an output argument. All output values are written in one operation.
struct assign_output_argument {
  std::shared_ptr<const argument> arg;
  std::vector<variant> values;
};

// An if/else statement.
struct branch {
  // Condition of the if statement.
  variant_ptr condition;
  // Statements if the condition is true:
  std::vector<variant> if_branch;
  // Statements if the condition is false:
  std::vector<variant> else_branch;

  template <typename T, typename = std::enable_if_t<std::is_constructible_v<ast::variant, T>>>
  branch(T&& arg, std::vector<variant>&& if_branch, std::vector<variant>&& else_branch)
      : condition{std::make_shared<const ast::variant>(std::forward<T>(arg))},
        if_branch(std::move(if_branch)),
        else_branch(std::move(else_branch)) {}
};

// Call a standard library function.
struct call {
  std_math_function function;
  std::vector<variant> args;

  template <typename... Args>
  explicit call(std_math_function function, Args&&... inputs)
      : function(function), args{std::forward<Args>(inputs)...} {}
};

// Cast a scalar from one numeric type to another.
struct cast {
  code_numeric_type destination_type;
  code_numeric_type source_type;
  variant_ptr arg;

  cast(code_numeric_type destination_type, code_numeric_type source_type, const variant_ptr& arg)
      : destination_type(destination_type), source_type(source_type), arg(arg) {}
};

// A relational comparison.
struct compare {
  relational_operation operation{};
  variant_ptr left;
  variant_ptr right;
};

// Construct a type from the provided arguments.
struct construct_return_value {
  argument_type type;
  std::vector<variant> args;

  construct_return_value(argument_type, std::vector<variant>&& args);
};

// Declare a new variable and optionally assign it a value.
struct declaration {
  // Name for the value being declared
  std::string name;
  // Type of the value:
  code_numeric_type type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  variant_ptr value{};

  declaration(std::string name, code_numeric_type type, variant_ptr value);

  // Construct w/ no rhs.
  declaration(std::string name, code_numeric_type type) : name(std::move(name)), type(type) {}
};

// Divide first operand by second operand.
struct divide {
  variant_ptr left;
  variant_ptr right;

  divide(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// Use a floating-point constant in the output code.
struct float_literal {
  double value;
};

// Signature and body of a function.
struct function_definition {
  function_signature signature;
  std::vector<ast::variant> body;

  function_definition(function_signature signature, std::vector<ast::variant> body);
};

// Access an input argument at a specific index.
struct input_value {
  std::shared_ptr<const argument> arg;
  index_t element;
};

// Use an integer constant in the output code.
struct integer_literal {
  std::int64_t value;
};

// Multiply two operands together.
struct multiply {
  variant_ptr left;
  variant_ptr right;

  multiply(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// A one-sided branch that assigns to an optional output, after checking for its existence.
// This corresponds to a block that looks something like:
//  if (<argument exists>) {
//    ... statements ...
//  }
struct optional_output_branch {
  // The argument this output corresponds to.
  std::shared_ptr<const argument> arg;

  // Statements in the if-branch.
  std::vector<variant> statements;

  explicit optional_output_branch(std::shared_ptr<const argument> arg,
                                  std::vector<variant>&& statements)
      : arg(std::move(arg)), statements(std::move(statements)) {}
};

// Use a symbolic constant in the output code.
struct special_constant {
  symbolic_constant_enum value;
};

// Create AST from the IR:
std::vector<ast::variant> create_ast(const wf::output_ir& ir, const function_signature& signature);

}  // namespace wf::ast
