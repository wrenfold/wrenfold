// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/types.h"
#include "wf/compound_expression.h"
#include "wf/expression.h"
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

  bool has_same_address(const argument& arg) const noexcept {
    return impl_.get() == arg.impl_.get();
  }

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

}  // namespace wf
