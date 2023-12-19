// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/code_generation/code_formatter.h"

namespace wf {

class cpp_code_generator {
 public:
  virtual ~cpp_code_generator() = default;

  std::string generate_code(const function_signature& signature,
                            const std::vector<ast::variant>& body) const;

  std::string operator()(const ast::add& x) const;

  std::string operator()(const ast::assign_output_argument& assignment) const;

  std::string operator()(const ast::assign_temporary& x) const;

  std::string operator()(const ast::branch& x) const;

  std::string operator()(const ast::call& x) const;

  std::string operator()(const ast::cast& x) const;

  std::string operator()(const ast::comment& x) const;

  std::string operator()(const ast::compare& x) const;

  std::string operator()(const ast::construct_matrix& x) const;

  std::string operator()(const ast::construct_custom_type& x) const;

  std::string operator()(const ast::declaration& x) const;

  std::string operator()(const ast::divide& x) const;

  std::string operator()(const ast::float_literal& x) const;

  std::string operator()(const ast::integer_literal& x) const;

  std::string operator()(const ast::multiply& x) const;

  std::string operator()(const ast::negate& x) const;

  std::string operator()(const ast::optional_output_branch& x) const;

  std::string operator()(const ast::read_input_scalar& x) const;

  std::string operator()(const ast::read_input_matrix& x) const;

  std::string operator()(const ast::read_input_struct& x) const;

  std::string operator()(const ast::return_value& x) const;

  std::string operator()(const ast::special_constant& x) const;

  std::string operator()(const ast::variable_ref& x) const;

  // We need a virtual method so that this behavior can be overriden on the python side.
  virtual std::string apply(const ast::variant& var) const;

  // Accept ast::variant and delegate formatting of the stored type to our derived class.
  // Using enable_if here to prevent implicit conversion to the variant type.
  template <typename T, typename = enable_if_same_t<T, ast::variant>>
  auto operator()(const T& var) const {
    return apply(var);
  }

  // Accept ast::variant_ptr
  auto operator()(const ast::variant_ptr& var) const {
    WF_ASSERT(var, "variant_ptr cannot be empty.");
    return apply(*var);
  }

 protected:
  // Create a fmt_view. All args will be forwarded back to the operator on this class that matches
  // them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::wf::make_fmt_view(*this, std::forward<Args>(args)...);
  }

  std::string format_signature(const function_signature& signature) const;
};

}  // namespace wf
