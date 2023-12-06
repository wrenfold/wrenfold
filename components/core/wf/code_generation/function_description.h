// Copyright 2023 Gareth Cross
#pragma once
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/code_generation/expression_group.h"
#include "wf/matrix_expression.h"

namespace wf {

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

  constexpr std::size_t size() const noexcept { return static_cast<std::size_t>(rows_ * cols_); }

  // Convert to [row, col] indices (assuming row major order).
  std::pair<index_t, index_t> compute_indices(std::size_t element) const {
    WF_ASSERT_LESS(element, size());
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

  argument(const std::string_view name, argument_type type, argument_direction direction)
      : name_(name), type_(std::move(type)), direction_(direction) {}

  // Name of the argument.
  constexpr const std::string& name() const noexcept { return name_; }

  // Type of the argument.
  constexpr const argument_type& type() const noexcept { return type_; }

  // Is the argument type a matrix.
  constexpr bool is_matrix() const noexcept { return std::holds_alternative<matrix_type>(type_); }

  // Is this argument optional? Presently only output arguments may be optional.
  constexpr bool is_optional() const noexcept {
    return direction_ == argument_direction::optional_output;
  }

  // Argument direction.
  constexpr argument_direction direction() const noexcept { return direction_; }

 private:
  std::string name_;
  argument_type type_;
  argument_direction direction_;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
struct function_signature {
 public:
  explicit function_signature(std::string name) noexcept : name_(std::move(name)) {}

  // Name of the function.
  constexpr const std::string& name() const noexcept { return name_; }

  // Number of arguments (both input and output).
  std::size_t num_arguments() const noexcept { return arguments_.size(); }

  // Push back a new argument.
  void add_argument(std::string_view name, argument_type type, argument_direction direction);

  // Find an argument by name.
  std::optional<std::shared_ptr<const argument>> argument_by_name(std::string_view str) const;

  // Get an argument by index.
  const std::shared_ptr<const argument>& argument_by_index(std::size_t index) const {
    WF_ASSERT_LESS(index, num_arguments());
    return arguments_[index];
  }

  // Access all arguments.
  constexpr const auto& arguments() const noexcept { return arguments_; }

  // Are any of the arguments to this function a matrix?
  bool has_matrix_arguments() const noexcept;

  // Get the type of the return value, if there is one.
  // If the function has no return value, this will be null.
  constexpr const std::optional<argument_type>& return_value_type() const noexcept {
    return return_value_type_;
  }

  // True if the return value type has been set.
  constexpr bool has_return_value() const noexcept { return return_value_type_.has_value(); }

  // Set the return value type.
  void set_return_value_type(argument_type type);

 private:
  std::string name_;
  std::vector<std::shared_ptr<const argument>> arguments_{};
  std::optional<argument_type> return_value_type_{};
};

// Store the signature of a function we will generate, plus all the captured output expressions.
// This type is a symbolic function description, which is then "transpiled" into an actual AST that
// can be written out as actual code.
// TODO: Use this in build_function_description?
struct function_description {
 public:
  // Stored in a shared ptr so that we can pass with no copies to and from python.
  using shared_ptr = std::shared_ptr<function_description>;

  // Construct with the name of the function.
  explicit function_description(std::string name) noexcept : signature_{std::move(name)} {}

  // Get function name.
  constexpr const std::string& name() const noexcept { return signature_.name(); }

  // Get the function signature.
  constexpr const function_signature& signature() const noexcept { return signature_; }

  // Get the vector of all output expressions, grouped by argument.
  constexpr const std::vector<expression_group>& output_expressions() const noexcept {
    return output_expressions_;
  }

  // Add an input argument to the function.
  // Returns the argument that the python side should pass to the user method.
  std::variant<Expr, MatrixExpr> add_input_argument(std::string_view name, argument_type type);

  // Record an output.
  void add_output_argument(std::string_view name, argument_type type, bool is_optional,
                           std::vector<Expr> expressions);

  // Set the return value.
  void set_return_value(argument_type type, std::vector<Expr> expressions);

 private:
  function_signature signature_;
  std::vector<expression_group> output_expressions_;
};

}  // namespace wf
