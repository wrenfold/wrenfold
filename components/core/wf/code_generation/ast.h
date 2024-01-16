// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "wf/checked_pointers.h"
#include "wf/code_generation/custom_function.h"
#include "wf/code_generation/function_description.h"
#include "wf/enumerations.h"

namespace wf::ast {

// clang-format off
using variant = std::variant<
    struct add,
    struct assign_temporary,
    struct assign_output_matrix,
    struct assign_output_scalar,
    struct assign_output_struct,
    struct branch,
    struct call_custom_function,
    struct call_std_function,
    struct cast,
    struct comment,
    struct compare,
    struct construct_custom_type,
    struct construct_matrix,
    struct declaration,
    struct divide,
    struct float_literal,
    struct get_argument,
    struct get_field,
    struct get_matrix_element,
    struct integer_literal,
    struct multiply,
    struct negate,
    struct optional_output_branch,
    struct special_constant,
    struct return_object,
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

  non_null<variant_ptr> left;
  non_null<variant_ptr> right;
};

// Assign a value to a temporary variable.
struct assign_temporary {
  static constexpr std::string_view snake_case_name_str = "assign_temporary";

  std::string left;
  non_null<variant_ptr> right;

  assign_temporary(std::string left, variant_ptr right);
};

// Assign to an output argument of matrix type.
struct assign_output_matrix {
  static constexpr std::string_view snake_case_name_str = "assign_output_matrix";
  argument arg;
  non_null<std::shared_ptr<const construct_matrix>> value;

  assign_output_matrix(argument arg, construct_matrix&& value);
};

// Assign to an output scalar argument.
struct assign_output_scalar {
  static constexpr std::string_view snake_case_name_str = "assign_output_scalar";
  argument arg;
  non_null<variant_ptr> value;
};

// Assign to an output argument that is a custom struct.
struct assign_output_struct {
  static constexpr std::string_view snake_case_name_str = "assign_output_struct";
  argument arg;
  non_null<std::shared_ptr<const construct_custom_type>> value;

  assign_output_struct(argument arg, construct_custom_type&& value);
};

// An if/else statement.
struct branch {
  static constexpr std::string_view snake_case_name_str = "branch";

  // Condition of the if statement.
  non_null<variant_ptr> condition;
  // Statements if the condition is true:
  std::vector<variant> if_branch;
  // Statements if the condition is false:
  std::vector<variant> else_branch;
};

// Call a custom function.
struct call_custom_function {
  static constexpr std::string_view snake_case_name_str = "call_custom_function";

  custom_function function;
  std::vector<variant> args;
};

// Call a standard math function.
struct call_std_function {
  static constexpr std::string_view snake_case_name_str = "call_std_function";

  std_math_function function;
  std::vector<variant> args;
};

// Cast a scalar from one numeric type to another.
struct cast {
  static constexpr std::string_view snake_case_name_str = "cast";

  code_numeric_type destination_type;
  code_numeric_type source_type;
  non_null<variant_ptr> arg;
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
  non_null<variant_ptr> left;
  non_null<variant_ptr> right;
};

// Construct a custom type.
struct construct_custom_type {
  static constexpr std::string_view snake_case_name_str = "construct_custom_type";

  // The type being constructed.
  custom_type type;
  // Vector of [field, ast] pairs that describe how to fill the fields of the output type.
  // Fields will be in the same order as in `type`.
  std::vector<std::tuple<std::string, ast::variant>> field_values;

  // Get a field by name (or nullptr if the field does not exist).
  maybe_null<const ast::variant*> get_field_by_name(std::string_view name) const;
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
  maybe_null<variant_ptr> value{nullptr};

  declaration(std::string name, code_numeric_type type, variant_ptr value);

  // Construct w/ no rhs.
  declaration(std::string name, code_numeric_type type);
};

// Divide first operand by second operand.
struct divide {
  static constexpr std::string_view snake_case_name_str = "divide";

  non_null<variant_ptr> left;
  non_null<variant_ptr> right;
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
  non_null<variant_ptr> arg;
  // Type being accessed.
  custom_type type;
  // Name of the field being accessed
  std::string field;
};

// Access a matrix element.
struct get_matrix_element {
  static constexpr std::string_view snake_case_name_str = "get_matrix_element";
  // Expression for the matrix we are accessing.
  non_null<variant_ptr> arg;
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

  non_null<variant_ptr> left;
  non_null<variant_ptr> right;
};

// Negate an operand.
struct negate {
  static constexpr std::string_view snake_case_name_str = "negate";

  non_null<variant_ptr> arg;
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
  std::vector<variant> statements;
};

// Use a symbolic constant in the output code.
struct special_constant {
  static constexpr std::string_view snake_case_name_str = "special_constant";

  symbolic_constant_enum value;
};

// A return statement.
struct return_object {
  static constexpr std::string_view snake_case_name_str = "return_object";

  non_null<variant_ptr> value;
};

// Usage of a variable.
struct variable_ref {
  static constexpr std::string_view snake_case_name_str = "variable_ref";

  // Name of the variable.
  std::string name;
};

// Types that are not part of the ast::variant, because they appear only in specific places in the
// output:

// A return type annotation in a function signature.
// The formatting of a type depends on the context in which it is used, which is why we bother to
// wrap type_variant in this object.
struct return_type_annotation {
  static constexpr std::string_view snake_case_name_str = "return_type_annotation";

  // The underlying type in the annotation, if the return type is non-void.
  std::optional<type_variant> type;
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

  // Return type annotation.
  constexpr const return_type_annotation& return_annotation() const noexcept {
    return return_type_;
  }

  constexpr const std::optional<type_variant>& return_type() const noexcept {
    return return_type_.type;
  }

  // Find an argument by name.
  std::optional<argument> argument_by_name(std::string_view str) const;

  // Get an argument by index.
  const argument& argument_by_index(std::size_t index) const {
    WF_ASSERT_LESS(index, num_arguments());
    return arguments_[index];
  }

  // True if any of the arguments is a matrix.
  bool has_matrix_arguments() const noexcept {
    return std::any_of(arguments_.begin(), arguments_.end(),
                       [](const argument& x) { return x.is_matrix(); });
  }

  // Get all the matrix arguments.
  std::vector<argument> matrix_args() const;

 private:
  // The name of the function.
  std::string name_{};

  // The return type of the function.
  return_type_annotation return_type_{};

  // Arguments to the function.
  std::vector<argument> arguments_{};
};

// Signature and function body.
class function_definition {
 public:
  static constexpr std::string_view snake_case_name_str = "function_definition";

  function_definition(function_signature signature, std::vector<ast::variant> body)
      : impl_(std::make_shared<const impl>(impl{std::move(signature), std::move(body)})) {}

  // Signature of the function.
  const function_signature& signature() const noexcept { return impl_->signature; }

  // Operations within the function body.
  const std::vector<ast::variant>& body() const noexcept { return impl_->body; }

 private:
  struct impl {
    // Signature of the function: float foo(float x, ...)
    function_signature signature;
    // Body of the function as a vector of statements.
    std::vector<ast::variant> body;
  };
  non_null<std::shared_ptr<const impl>> impl_;
};

// Types that don't appear in ast::variant, but which must be exposed via our python wrapper so that
// the user can override their formatting.
// clang-format off
using extra_ast_types = type_list<
  argument,
  custom_type,
  function_definition,
  function_signature,
  return_type_annotation
>;
// clang-format on
using all_ast_types =
    type_list_concatenate_t<type_list_from_variant_t<ast::variant>, extra_ast_types>;

// Make a shared-ptr to ast::variant.
template <typename T, typename... Args>
ast::variant_ptr make_shared_variant(Args&&... args) {
  return std::make_shared<const ast::variant>(T{std::forward<Args>(args)...});
}

}  // namespace wf::ast
