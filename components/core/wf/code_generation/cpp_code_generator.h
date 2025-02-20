// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string>
#include <string_view>

#include "wf/code_generation/ast.h"
#include "wf/utility/strings.h"

namespace wf {

enum class cpp_matrix_type_behavior {
  // Use spans to pass matrices as input and output arguments.
  // Matrix arguments will be generic types that must implement wf::convert_to_span.
  generic_span,
  // Use Eigen types to pass matrices as input and output arguments, and return values.
  // Matrix arguments will be Eigen maps
  eigen
};

// Generate C++ code.
class cpp_code_generator {
 public:
  constexpr explicit cpp_code_generator(cpp_matrix_type_behavior behavior) noexcept
      : behavior_(behavior) {}

  virtual ~cpp_code_generator() = default;

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

  // Accept `ast_element`.
  std::string operator()(const ast::ast_element& element) const;

  // Wrap generated function code in a preamble with includes and a namespace.
  std::string apply_preamble(std::string_view code, std::string_view ns,
                             std::string_view imports = "") const;

 protected:
  cpp_matrix_type_behavior behavior_;

  // Create a fmt_view. All args will be forwarded back to the operator on this class that matches
  // them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::wf::make_fmt_view(*this, std::forward<Args>(args)...);
  }
};

}  // namespace wf
