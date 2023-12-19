// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "wf/code_generation/function_description.h"
#include "wf/enumerations.h"

namespace wf::ast {

// clang-format off
using variant = std::variant<
    struct add,
    struct assign_temporary,
    struct assign_output_argument,
    struct branch,
    struct call,
    struct cast,
    struct comment,
    struct compare,
    struct construct_custom_type,
    struct construct_matrix,
    struct declaration,
    struct divide,
    struct float_literal,
    struct integer_literal,
    struct multiply,
    struct negate,
    struct optional_output_branch,
    struct special_constant,
    struct read_input_scalar,
    struct read_input_matrix,
    struct read_input_struct,
    struct return_value,
    struct variable_ref
    >;
// clang-format on

using variant_vector = std::vector<variant>;

// This is a shared_ptr so that we can copy AST members.
// Copying is desirable to satisfy the pybind11 wrapper.
using variant_ptr = std::shared_ptr<const variant>;

// Add two values together.
struct add {
  static constexpr std::string_view snake_case_name_str = "add";

  variant_ptr left;
  variant_ptr right;

  add(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// Assign a value to a temporary variable.
struct assign_temporary {
  static constexpr std::string_view snake_case_name_str = "assign_temporary";

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
  static constexpr std::string_view snake_case_name_str = "assign_output_argument";

  std::shared_ptr<const argument> arg;
  std::vector<variant> values;
};

// An if/else statement.
struct branch {
  static constexpr std::string_view snake_case_name_str = "branch";

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
  static constexpr std::string_view snake_case_name_str = "call";

  std_math_function function;
  std::vector<variant> args;

  template <typename... Args>
  explicit call(const std_math_function function, Args&&... inputs)
      : function(function), args{std::forward<Args>(inputs)...} {}
};

// Cast a scalar from one numeric type to another.
struct cast {
  static constexpr std::string_view snake_case_name_str = "cast";

  code_numeric_type destination_type;
  code_numeric_type source_type;
  variant_ptr arg;

  cast(code_numeric_type destination_type, code_numeric_type source_type, const variant_ptr& arg)
      : destination_type(destination_type), source_type(source_type), arg(arg) {}
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
  variant_ptr left;
  variant_ptr right;
};

// Construct a custom type.
struct construct_custom_type {
  static constexpr std::string_view snake_case_name_str = "construct_custom_type";

  // The type being constructed.
  custom_type::const_shared_ptr type;
  // Vector of [field, ast] pairs that describe how to fill the fields of the output type.
  // Fields will be in the same order as in `type`.
  std::vector<std::tuple<std::string, ast::variant>> field_values;
};

// Construct a matrix type from arguments.
struct construct_matrix {
  static constexpr std::string_view snake_case_name_str = "construct_matrix";

  matrix_type type;
  std::vector<variant> args;
};

// Declare a new variable and optionally assign it a value.
struct declaration {
  static constexpr std::string_view snake_case_name_str = "declaration";

  // Name for the value being declared
  std::string name;
  // Type of the value:
  code_numeric_type type;
  // Right hand side of the declaration (empty if the value is computed later).
  // If a value is assigned, then the result can be presumed to be constant.
  variant_ptr value{};

  declaration(std::string name, code_numeric_type type, variant_ptr value)
      : name(std::move(name)), type(type), value(std::move(value)) {}

  // Construct w/ no rhs.
  declaration(std::string name, code_numeric_type type) : name(std::move(name)), type(type) {}
};

// Divide first operand by second operand.
struct divide {
  static constexpr std::string_view snake_case_name_str = "divide";

  variant_ptr left;
  variant_ptr right;

  divide(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// Use a floating-point constant in the output code.
struct float_literal {
  static constexpr std::string_view snake_case_name_str = "float_literal";

  double value;
};

// Use an integer constant in the output code.
struct integer_literal {
  static constexpr std::string_view snake_case_name_str = "integer_literal";

  std::int64_t value;
};

// Multiply two operands together.
struct multiply {
  static constexpr std::string_view snake_case_name_str = "multiply";

  variant_ptr left;
  variant_ptr right;

  multiply(variant_ptr left, variant_ptr right) : left(std::move(left)), right(std::move(right)) {}
};

// Negate an operand.
struct negate {
  static constexpr std::string_view snake_case_name_str = "negate";

  variant_ptr arg;

  explicit negate(variant_ptr arg) noexcept : arg(std::move(arg)) {}
};

// A one-sided branch that assigns to an optional output, after checking for its existence.
// This corresponds to a block that looks something like:
//  if (<argument exists>) {
//    ... statements ...
//  }
struct optional_output_branch {
  static constexpr std::string_view snake_case_name_str = "optional_output_branch";

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
  static constexpr std::string_view snake_case_name_str = "special_constant";

  symbolic_constant_enum value;
};

// Access a scalar input argument.
struct read_input_scalar {
  static constexpr std::string_view snake_case_name_str = "read_input_scalar";

  std::shared_ptr<const argument> arg;
};

// Access a single element from a matrix (2D span) input argument.
struct read_input_matrix {
  static constexpr std::string_view snake_case_name_str = "read_input_matrix";

  std::shared_ptr<const argument> arg;
  index_t row;
  index_t col;
};

// Access a single scalar member from a custom input type.
struct read_input_struct {
  static constexpr std::string_view snake_case_name_str = "read_input_struct";

  std::shared_ptr<const argument> arg;
  // Sequence of nested accessors required to obtain the relevant member.
  std::vector<access_variant> access_sequence{};
};

// A return statement.
struct return_value {
  static constexpr std::string_view snake_case_name_str = "return_value";

  variant_ptr value;
};

// Usage of a variable.
struct variable_ref {
  static constexpr std::string_view snake_case_name_str = "variable_ref";

  // Name of the variable.
  std::string name;
};

}  // namespace wf::ast
