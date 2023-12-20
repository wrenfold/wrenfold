// Copyright 2023 Gareth Cross
#pragma once
#include <variant>
#include <vector>

#include "wf/assertions.h"
#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/types.h"
#include "wf/matrix_expression.h"

namespace wf {

// Specify how an argument is used (input, output).
enum class argument_direction {
  input,
  output,
  optional_output,
};

// Store an argument to a function.
class argument {
 public:
  argument(const std::string_view name, type_variant type, argument_direction direction,
           const std::size_t index)
      : impl_(std::make_shared<const impl>(
            impl{std::string(name), std::move(type), direction, index})) {}

  // Name of the argument.
  const std::string& name() const noexcept { return impl_->name; }

  // Type of the argument.
  const type_variant& type() const noexcept { return impl_->type; }

  // Is the argument type a matrix.
  bool is_matrix() const noexcept { return std::holds_alternative<matrix_type>(impl_->type); }

  // Is this argument optional? Presently only output arguments may be optional.
  bool is_optional() const noexcept {
    return impl_->direction == argument_direction::optional_output;
  }

  // Argument direction.
  argument_direction direction() const noexcept { return impl_->direction; }

  // Position of this argument in the argument list.
  std::size_t index() const noexcept { return impl_->index; }

 private:
  // We share arguments in multiple places, and return them into python.
  // For that reason we place the implementation in a shared_ptr to const.
  struct impl {
    std::string name;
    type_variant type;
    argument_direction direction;
    std::size_t index;
  };
  std::shared_ptr<const impl> impl_;
};

// Describe a function signature.
// Stores a name, and type+name information for all the arguments.
class function_signature {
 public:
  explicit function_signature(std::string name) noexcept : name_(std::move(name)) {}

  // Name of the function.
  constexpr const std::string& name() const noexcept { return name_; }

  // Number of arguments (both input and output).
  std::size_t num_arguments() const noexcept { return arguments_.size(); }

  // Push back a new argument.
  void add_argument(std::string_view name, type_variant type, argument_direction direction);

  // Find an argument by name.
  std::optional<argument> argument_by_name(std::string_view str) const;

  // Get an argument by index.
  const argument& argument_by_index(std::size_t index) const {
    WF_ASSERT_LESS(index, num_arguments());
    return arguments_[index];
  }

  // Access all arguments.
  constexpr const auto& arguments() const noexcept { return arguments_; }

  // Are any of the arguments to this function a matrix?
  bool has_matrix_arguments() const noexcept;

  // Get the type of the return value, if there is one.
  // If the function has no return value, this will be null.
  constexpr const std::optional<type_variant>& return_value_type() const noexcept {
    return return_value_type_;
  }

  // True if the return value type has been set.
  constexpr bool has_return_value() const noexcept { return return_value_type_.has_value(); }

  // Set the return value type.
  void set_return_value_type(type_variant type);

 private:
  std::string name_;
  std::vector<argument> arguments_{};
  std::optional<type_variant> return_value_type_{};
};

// Store the signature of a function we will generate, plus all the captured output expressions.
// This type is a symbolic function description, which is then "transpiled" into an actual AST that
// can be written out as actual code.
// TODO: Use this in build_function_description?
class function_description {
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
  // For custom types, we return a vector of expressions and the python side must map these to
  // fields on the user's custom type.
  std::variant<Expr, MatrixExpr, std::vector<Expr>> add_input_argument(std::string_view name,
                                                                       type_variant type);

  // Record an output.
  void add_output_argument(std::string_view name, type_variant type, bool is_optional,
                           std::vector<Expr> expressions);

  // Set the return value. Only one return value is presently supported, so this may only be invoked
  // once.
  void set_return_value(type_variant type, std::vector<Expr> expressions);

 private:
  function_signature signature_;
  std::vector<expression_group> output_expressions_;
};

}  // namespace wf
