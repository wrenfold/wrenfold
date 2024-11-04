// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>

#include "wf/code_generation/ast.h"
#include "wf/utility/strings.h"

namespace wf {

enum class python_generator_target {
  // Target the numpy API.
  numpy,
  // Target the PyTorch API.
  pytorch,
  // Target the JAX API.
  jax,
};

enum class python_generator_float_width {
  // Assume arrays are float32.
  float32,
  // Assume arrays are float64.
  float64,
};

// Generate Python code for use with: NumPy, PyTorch, and JAX.
// TODO: Add support for Numba as well.
class python_code_generator {
 public:
  explicit python_code_generator(python_generator_target target,
                                 python_generator_float_width float_width, int indent);

  virtual ~python_code_generator() = default;

  // Type formatters
  virtual std::string operator()(const scalar_type& scalar) const;
  virtual std::string operator()(const matrix_type& matrix) const;
  virtual std::string operator()(const custom_type& custom) const;

  virtual std::string operator()(const argument& arg) const;

  virtual std::string operator()(const ast::function_definition& definition) const;

  virtual std::string operator()(const ast::function_signature& signature) const;

  virtual std::string operator()(const ast::add& x) const;

  virtual std::string operator()(const ast::assign_output_matrix& x) const;

  virtual std::string operator()(const ast::assign_output_scalar& x) const;

  virtual std::string operator()(const ast::assign_output_struct& x) const;

  virtual std::string operator()(const ast::assign_temporary& x) const;

  virtual std::string operator()(const ast::boolean_literal& x) const;

  virtual std::string operator()(const ast::branch& x) const;

  virtual std::string operator()(const ast::call_external_function& x) const;

  virtual std::string operator()(const ast::call_std_function& x) const;

  virtual std::string operator()(const ast::cast& x) const;

  virtual std::string operator()(const ast::comment& x) const;

  virtual std::string operator()(const ast::compare& x) const;

  virtual std::string operator()(const ast::construct_matrix& x) const;

  virtual std::string operator()(const ast::construct_custom_type& x) const;

  virtual std::string operator()(const ast::declaration& x) const;

  virtual std::string operator()(const ast::divide& x) const;

  virtual std::string operator()(const ast::float_literal& x) const;

  virtual std::string operator()(const ast::get_argument& x) const;

  virtual std::string operator()(const ast::get_field& x) const;

  virtual std::string operator()(const ast::get_matrix_element& x) const;

  virtual std::string operator()(const ast::integer_literal& x) const;

  virtual std::string operator()(const ast::multiply& x) const;

  virtual std::string operator()(const ast::negate& x) const;

  virtual std::string operator()(const ast::optional_output_branch& x) const;

  virtual std::string operator()(const ast::parenthetical& x) const;

  virtual std::string operator()(const ast::return_object& x) const;

  virtual std::string operator()(const ast::special_constant& x) const;

  virtual std::string operator()(const ast::ternary& x) const;

  virtual std::string operator()(const ast::variable_ref& x) const;

  std::string operator()(const ast::ast_element& element) const;

  constexpr python_generator_target target() const noexcept { return target_; }
  constexpr python_generator_float_width float_width() const noexcept { return float_width_; }
  constexpr std::size_t indentation() const noexcept { return indent_; }

 protected:
  python_generator_target target_;
  python_generator_float_width float_width_;
  std::size_t indent_;

  // Create a fmt_view. All args will be forwarded back to the operator on this class that matches
  // them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::wf::make_fmt_view(*this, std::forward<Args>(args)...);
  }
};

}  // namespace wf
