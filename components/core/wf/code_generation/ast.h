// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <memory>
#include <string>
#include <vector>

#include "wf/checked_pointers.h"
#include "wf/code_generation/ast_element.h"
#include "wf/code_generation/function_description.h"
#include "wf/enumerations.h"
#include "wf/external_function.h"

namespace wf::ast {

// Add two values together.
struct add {
  static constexpr std::string_view snake_case_name_str = "add";

  ast_element left;
  ast_element right;
};

// Assign a value to a temporary variable.
struct assign_temporary {
  static constexpr std::string_view snake_case_name_str = "assign_temporary";

  std::string left;
  ast_element right;

  assign_temporary(std::string left, ast_element right)
      : left{std::move(left)}, right{std::move(right)} {}
};

// Assign to an output scalar argument.
struct assign_output_scalar {
  static constexpr std::string_view snake_case_name_str = "assign_output_scalar";
  argument arg;
  ast_element value;
};

// A boolean litera: 'true' or 'false'.
struct boolean_literal {
  static constexpr std::string_view snake_case_name_str = "boolean_literal";
  bool value;
};

// An if/else statement.
struct branch {
  static constexpr std::string_view snake_case_name_str = "branch";

  // Condition of the if statement.
  ast_element condition;
  // Statements if the condition is true:
  std::vector<ast_element> if_branch;
  // Statements if the condition is false:
  std::vector<ast_element> else_branch;
};

// Call an external function.
struct call_external_function {
  static constexpr std::string_view snake_case_name_str = "call_external_function";

  external_function function;
  std::vector<ast_element> args;
};

// Call a standard math function.
struct call_std_function {
  static constexpr std::string_view snake_case_name_str = "call_std_function";

  std_math_function function;
  std::vector<ast_element> args;

  // Access args with bounds checking.
  const ast_element& operator[](std::size_t i) const {
    WF_ASSERT_LESS(i, args.size());
    return args[i];
  }
};

// Cast a scalar from one numeric type to another.
struct cast {
  static constexpr std::string_view snake_case_name_str = "cast";

  code_numeric_type destination_type;
  code_numeric_type source_type;
  ast_element arg;
};

// A comment.
struct comment {
  static constexpr std::string_view snake_case_name_str = "comment";

  std::string content;

  // Split the string by line breaks.
  // Trailing newlines are stripped from each returned string.
  std::vector<std::string> split_lines() const;
};

// A relational comparison.
struct compare {
  static constexpr std::string_view snake_case_name_str = "compare";

  relational_operation operation{};
  ast_element left;
  ast_element right;
};

// Construct a custom type.
struct construct_custom_type {
  static constexpr std::string_view snake_case_name_str = "construct_custom_type";

  // The type being constructed.
  custom_type type;

  // Vector of [field, ast] pairs that describe how to fill the fields of the output type.
  // Fields will be in the same order as in `type`.
  std::vector<std::tuple<std::string, ast_element>> field_values;

  // Get a field by name (or nullptr if the field does not exist).
  maybe_null<const ast_element*> get_field_by_name(std::string_view name) const;
};

// Construct a matrix type from arguments.
struct construct_matrix {
  static constexpr std::string_view snake_case_name_str = "construct_matrix";

  matrix_type type;
  std::vector<ast_element> args;
};

// Declare a new variable and optionally assign it a value.
struct declaration {
  static constexpr std::string_view snake_case_name_str = "declaration";

  // Name for the value being declared
  std::string name;
  // Type of the value:
  type_variant type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  std::optional<ast_element> value{std::nullopt};

  declaration(std::string name, type_variant type, ast_element value)
      : name(std::move(name)), type(std::move(type)), value(std::move(value)) {}

  // Construct w/ no rhs.
  declaration(std::string name, type_variant type) : name(std::move(name)), type(std::move(type)) {}
};

// Divide first operand by second operand.
struct divide {
  static constexpr std::string_view snake_case_name_str = "divide";

  ast_element left;
  ast_element right;
};

// Use a floating-point constant in the output code.
struct float_literal {
  static constexpr std::string_view snake_case_name_str = "float_literal";

  double value;
};

// Refer to an input argument.
struct get_argument {
  static constexpr std::string_view snake_case_name_str = "get_argument";
  argument arg;
};

// Access a field on a struct.
struct get_field {
  static constexpr std::string_view snake_case_name_str = "get_field";
  // Expression for the struct we are accessing.
  ast_element arg;
  // Type being accessed.
  custom_type type;
  // Name of the field being accessed
  std::string field;
};

// Access a matrix element.
struct get_matrix_element {
  static constexpr std::string_view snake_case_name_str = "get_matrix_element";
  // Expression for the matrix we are accessing.
  ast_element arg;
  // Row and column.
  index_t row;
  index_t col;
};

// Use an integer constant in the output code.
struct integer_literal {
  static constexpr std::string_view snake_case_name_str = "integer_literal";

  std::int64_t value;
};

// Multiply two operands together.
struct multiply {
  static constexpr std::string_view snake_case_name_str = "multiply";

  ast_element left;
  ast_element right;
};

// Negate an operand.
struct negate {
  static constexpr std::string_view snake_case_name_str = "negate";
  ast_element arg;
};

// A one-sided branch that assigns to an optional output, after checking for its existence.
// This corresponds to a block that looks something like:
//  if (<argument exists>) {
//    ... statements ...
//  }
struct optional_output_branch {
  static constexpr std::string_view snake_case_name_str = "optional_output_branch";

  // The argument this output corresponds to.
  argument arg;

  // Statements in the if-branch.
  std::vector<ast_element> statements;
};

// Use a symbolic constant in the output code.
struct special_constant {
  static constexpr std::string_view snake_case_name_str = "special_constant";

  symbolic_constant_enum value;
};

// A return statement.
struct return_object {
  static constexpr std::string_view snake_case_name_str = "return_object";
  ast_element value;
};

// Usage of a variable.
struct variable_ref {
  static constexpr std::string_view snake_case_name_str = "variable_ref";

  // Name of the variable.
  std::string name;
};

// Assign to an output argument of matrix type.
// Out of alphabetical order, but needs the definition of `construct_matrix` to be visible.
struct assign_output_matrix {
  static constexpr std::string_view snake_case_name_str = "assign_output_matrix";
  argument arg;
  construct_matrix value;
};

// Assign to an output argument that is a custom struct.
// Out of alphabetical order, but needs the definition of `construct_custom_type` to be visible.
struct assign_output_struct {
  static constexpr std::string_view snake_case_name_str = "assign_output_struct";
  argument arg;
  construct_custom_type value;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
struct function_signature {
  static constexpr std::string_view snake_case_name_str = "function_signature";

  function_signature(std::string name, std::optional<type_variant> return_type,
                     std::vector<argument> arguments)
      : name_(std::move(name)),
        return_type_{std::move(return_type)},
        arguments_(std::move(arguments)) {}

  // Name of the function.
  constexpr const std::string& name() const noexcept { return name_; }

  // Number of arguments (both input and output).
  std::size_t num_arguments() const noexcept { return arguments_.size(); }

  // Access all arguments.
  constexpr const auto& arguments() const noexcept { return arguments_; }

  // Return type.
  constexpr const std::optional<type_variant>& return_type() const noexcept { return return_type_; }

  // Find an argument by name.
  std::optional<argument> argument_by_name(std::string_view str) const;

  // Get an argument by index.
  const argument& argument_by_index(std::size_t index) const {
    WF_ASSERT_LESS(index, num_arguments());
    return arguments_[index];
  }

  // True if any of the arguments is a matrix.
  bool has_matrix_arguments() const noexcept {
    return any_of(arguments_, [](const argument& x) { return x.is_matrix(); });
  }

  // Get all the matrix arguments.
  std::vector<argument> matrix_args() const;

 private:
  // The name of the function.
  std::string name_{};

  // The return type of the function.
  std::optional<type_variant> return_type_{};

  // Arguments to the function.
  std::vector<argument> arguments_{};
};

// Signature and function body.
class function_definition {
 public:
  static constexpr std::string_view snake_case_name_str = "function_definition";

  function_definition(function_signature signature, std::vector<ast::ast_element> body)
      : impl_(std::make_shared<const impl>(impl{std::move(signature), std::move(body)})) {}

  // Signature of the function.
  const function_signature& signature() const noexcept { return impl_->signature; }

  // Operations within the function body.
  const std::vector<ast::ast_element>& body() const noexcept { return impl_->body; }

 private:
  struct impl {
    // Signature of the function: float foo(float x, ...)
    function_signature signature;
    // Body of the function as a vector of statements.
    std::vector<ast::ast_element> body;
  };
  non_null<std::shared_ptr<const impl>> impl_;
};

}  // namespace wf::ast
