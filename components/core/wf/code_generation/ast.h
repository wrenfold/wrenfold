// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/enumerations.h"
#include "wf/hashing.h"

namespace math {
class flat_ir;
class output_ir;
}  // namespace math

namespace math::ast {

// Represent a scalar argument type (float, int, etc).
class scalar_type {
 public:
  explicit constexpr scalar_type(code_numeric_type numeric_type) noexcept
      : numeric_type_(numeric_type) {}

  constexpr code_numeric_type numeric_type() const noexcept { return numeric_type_; }

 private:
  code_numeric_type numeric_type_;
};

// Represent a matrix argument type. The dimensions are known at generation time.
class matrix_type {
 public:
  constexpr matrix_type(index_t rows, index_t cols) noexcept : rows_(rows), cols_(cols) {}

  constexpr index_t rows() const noexcept { return rows_; }
  constexpr index_t cols() const noexcept { return cols_; }

  // Convert to [row, col] indices (assuming row major order).
  std::pair<index_t, index_t> compute_indices(std::size_t element) const {
    WF_ASSERT_LESS(element, static_cast<std::size_t>(rows_) * static_cast<std::size_t>(cols_));
    return std::make_pair(static_cast<index_t>(element) / cols_,
                          static_cast<index_t>(element) % cols_);
  }

 private:
  index_t rows_;
  index_t cols_;
};

// TODO: Add ability to add custom type.
using argument_type = std::variant<scalar_type, matrix_type>;

// Specify how an argument is used (input, output).
enum class argument_direction {
  input,
  output,
  optional_output,
};

// Store an argument to a function.
class argument {
 public:
  using shared_ptr = std::shared_ptr<const argument>;

  argument(const std::string_view name, ast::argument_type type, argument_direction direction)
      : name_(name), type_(std::move(type)), direction_(direction) {}

  // Name of the argument.
  constexpr const std::string& name() const noexcept { return name_; }

  // Type of the argument.
  constexpr const ast::argument_type& type() const noexcept { return type_; }

  // Is the argument type a matrix.
  constexpr bool is_matrix() const noexcept {
    return std::holds_alternative<ast::matrix_type>(type_);
  }

  // Is this argument optional? Presently only output arguments may be optional.
  constexpr bool is_optional() const noexcept {
    return direction_ == argument_direction::optional_output;
  }

  // Argument direction.
  constexpr argument_direction direction() const noexcept { return direction_; }

 private:
  std::string name_;
  ast::argument_type type_;
  argument_direction direction_;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
struct function_signature {
 public:
  explicit function_signature(std::string name) noexcept : function_name(std::move(name)) {}

  template <typename... Args>
  void add_argument(Args&&... args) {
    arguments.push_back(std::make_shared<const ast::argument>(std::forward<Args>(args)...));
  }

  // Find an argument by name.
  const std::shared_ptr<const ast::argument>& get_argument(const std::string_view str) const {
    auto it = std::find_if(arguments.begin(), arguments.end(),
                           [&](const auto& arg) { return arg->name() == str; });
    WF_ASSERT(it != arguments.end(), "Argument does not exist: {}", str);
    return *it;
  }

  // Are any of the arguments to this function a matrix?
  bool has_matrix_arguments() const noexcept {
    return std::any_of(arguments.begin(), arguments.end(),
                       [](const auto& arg) { return arg->is_matrix(); });
  }

  std::string function_name;
  std::vector<std::shared_ptr<const ast::argument>> arguments{};
  std::optional<ast::argument_type> return_value;
};

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
  std::shared_ptr<const argument> argument;
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
  ast::argument_type type;
  std::vector<variant> args;

  construct_return_value(ast::argument_type, std::vector<variant>&& args);
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
  std::shared_ptr<const argument> argument;
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
  symbolic_constants value;
};

// Create AST from the IR:
std::vector<ast::variant> create_ast(const math::output_ir& ir,
                                     const function_signature& signature);

// method definitions:

inline function_definition::function_definition(function_signature signature,
                                                std::vector<ast::variant> body)
    : signature(std::move(signature)), body(std::move(body)) {}

inline construct_return_value::construct_return_value(ast::argument_type type,
                                                      std::vector<ast::variant>&& args)
    : type(type), args(std::move(args)) {}

inline declaration::declaration(std::string name, code_numeric_type type, variant_ptr value)
    : name(std::move(name)), type(type), value(std::move(value)) {}

}  // namespace math::ast
