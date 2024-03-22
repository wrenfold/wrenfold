// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/string_utils.h"

namespace wf {

// Generate rust code.
class rust_code_generator {
 public:
  virtual ~rust_code_generator() = default;

  virtual std::string operator()(const argument& arg) const;

  virtual std::string operator()(const custom_type& custom) const;

  virtual std::string operator()(const ast::function_definition& definition) const;

  virtual std::string operator()(const ast::function_signature& signature) const;

  virtual std::string operator()(const ast::return_type_annotation& x) const;

  // Types from the ast::variant:

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

  virtual std::string operator()(const ast::declaration_type_annotation& x) const;

  virtual std::string operator()(const ast::divide& x) const;

  virtual std::string operator()(const ast::float_literal& x) const;

  virtual std::string operator()(const ast::get_argument& x) const;

  virtual std::string operator()(const ast::get_field& x) const;

  virtual std::string operator()(const ast::get_matrix_element& x) const;

  virtual std::string operator()(const ast::integer_literal& x) const;

  virtual std::string operator()(const ast::multiply& x) const;

  virtual std::string operator()(const ast::negate& x) const;

  virtual std::string operator()(const ast::optional_output_branch& x) const;

  virtual std::string operator()(const ast::return_object& x) const;

  virtual std::string operator()(const ast::special_constant& x) const;

  virtual std::string operator()(const ast::variable_ref& x) const;

  // Accept `ast_element`.
  std::string operator()(const ast::ast_element& element) const;

 protected:
  // Create a fmt_view that can be passed to code_formatter. All args will be
  // forwarded back to the operator on this class that matches them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::wf::make_fmt_view(*this, std::forward<Args>(args)...);
  }
};

}  // namespace wf
