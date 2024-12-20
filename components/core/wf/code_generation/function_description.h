// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <variant>
#include <vector>

#include "wf/code_generation/expression_group.h"
#include "wf/code_generation/type_registry.h"
#include "wf/code_generation/types.h"
#include "wf/matrix_expression.h"
#include "wf/utility/checked_pointers.h"

namespace wf {
class variable_creator;  //  Fwd declare.

// Specify how an argument is used (input, output).
enum class argument_direction {
  input,
  output,
  optional_output,
};

// Store an argument to a function.
class argument {
 public:
  static constexpr std::string_view snake_case_name_str = "argument";

  argument(std::string_view name, type_variant type, argument_direction direction,
           std::size_t index);

  // Name of the argument.
  const std::string& name() const noexcept { return impl_->name; }

  // Type of the argument.
  const type_variant& type() const noexcept { return impl_->type; }

  // Is the argument type a matrix.
  bool is_matrix() const noexcept { return std::holds_alternative<matrix_type>(impl_->type); }

  // Is the argument type a custom user-specified struct.
  bool is_custom_type() const noexcept { return std::holds_alternative<custom_type>(impl_->type); }

  // Is this argument optional? Presently only output arguments may be optional.
  bool is_optional() const noexcept {
    return impl_->direction == argument_direction::optional_output;
  }

  // Is this an input argument?
  bool is_input() const noexcept { return impl_->direction == argument_direction::input; }

  // Argument direction.
  argument_direction direction() const noexcept { return impl_->direction; }

  // Position of this argument in the argument list.
  std::size_t index() const noexcept { return impl_->index; }

  // Create symbolic input expressions that go with this argument.
  std::variant<scalar_expr, matrix_expr, compound_expr> create_symbolic_input() const;

 private:
  // We share arguments in multiple places, and return them into python.
  // For that reason we place the implementation in a shared_ptr to const.
  struct impl {
    std::string name;
    type_variant type;
    argument_direction direction;
    std::size_t index;
  };
  non_null<std::shared_ptr<const impl>> impl_;
};

template <>
struct hash_struct<argument> {
  std::size_t operator()(const argument& arg) const noexcept;
};

template <>
struct is_identical_struct<argument> {
  bool operator()(const argument& a, const argument& b) const;
};

// Store the signature of a function we will generate, plus all the captured output expressions.
// This type is a symbolic function description, which is then "transpiled" into an actual AST that
// can be written out as actual code.
class function_description {
 public:
  // Construct with the name of the function.
  explicit function_description(std::string name) noexcept;

  // Get function name.
  const std::string& name() const noexcept { return impl_->name; }

  // Get the function arguments.
  const std::vector<argument>& arguments() const noexcept { return impl_->arguments; }

  // Get the return type.
  const std::optional<type_variant>& return_value_type() const noexcept {
    return impl_->return_value_type;
  }

  // Retrieve output expressions for the given key.
  const any_expression& find_output_expressions(const output_key& key) const;

  // Retrieve all output expressions.
  const auto& output_expressions() const noexcept { return impl_->output_expressions; }

  // Add an input argument to the function.
  // Returns the argument that the python side should pass to the user method.
  // For custom types, we return a vector of expressions and the python side must map these to
  // fields on the user's custom type.
  std::variant<scalar_expr, matrix_expr, compound_expr> add_input_argument(std::string_view name,
                                                                           type_variant type);

  // Record an output.
  void add_output_argument(std::string_view name, type_variant type, bool is_optional,
                           any_expression expression);

  // Set the return value. Only one return value is presently supported, so this may only be invoked
  // once.
  void set_return_value(type_variant type, any_expression expression);

  // Add a `return_value` to this signature.
  template <typename Value, typename Type>
  void add_output_value(const return_value<Value>& value, Type type) {
    any_expression expression = detail::extract_function_output(type, value.value());
    set_return_value(std::move(type), std::move(expression));
  }

  // Add an `output_arg` output to this function.
  template <typename Value, typename Type>
  void add_output_value(const output_arg<Value>& value, Type type) {
    any_expression expression = detail::extract_function_output(type, value.value());
    add_output_argument(value.name(), std::move(type), value.is_optional(), std::move(expression));
  }

 private:
  const argument& add_argument(std::string_view name, type_variant type,
                               argument_direction direction);

  struct impl {
    std::string name;
    std::vector<argument> arguments{};
    std::optional<type_variant> return_value_type{};

    // Map from: output (type, name) --> <expression values>
    std::unordered_map<output_key, any_expression, hash_struct<output_key>> output_expressions{};

    explicit impl(std::string&& name) noexcept : name(std::move(name)) {}
  };

  // Shared pointer so we can share this in python with no copies.
  non_null<std::shared_ptr<impl>> impl_;
};

}  // namespace wf
