// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/code_formatter.h"

#include "wf/assertions.h"

namespace wf {

// Generate rust code.
class rust_code_generator {
 public:
  std::string generate_code(const ast::function_signature& signature,
                            const std::vector<ast::variant>& body) const;

  // Create a fmt_view that can be passed to code_formatter. All args will be
  // forwarded back to the operator on this class that matches them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::wf::make_fmt_view(*this, std::forward<Args>(args)...);
  }

  void operator()(code_formatter& formatter, const ast::add& x) const;

  void operator()(code_formatter& formatter, const ast::assign_output_argument& x) const;

  void operator()(code_formatter& formatter, const ast::assign_temporary& x) const;

  void operator()(code_formatter& formatter, const ast::branch& x) const;

  void operator()(code_formatter& formatter, const ast::call& x) const;

  void operator()(code_formatter& formatter, const ast::cast& x) const;

  void operator()(code_formatter& formatter, const ast::compare& x) const;

  void operator()(code_formatter& formatter, const ast::construct_return_value& x) const;

  void operator()(code_formatter& formatter, const ast::declaration& x) const;

  void operator()(code_formatter& formatter, const ast::divide& x) const;

  void operator()(code_formatter& formatter, const ast::float_literal& x) const {
    formatter.format("{}f64", x.value);
  }

  void operator()(code_formatter& formatter, const ast::special_constant& x) const;

  void operator()(code_formatter& formatter, const ast::variable_ref& x) const {
    formatter.format(x.name);
  }

  void operator()(code_formatter& formatter, const ast::input_value& x) const;

  void operator()(code_formatter& formatter, const ast::integer_literal& x) const {
    formatter.format("{}i64", x.value);
  }

  void operator()(code_formatter& formatter, const ast::multiply& x) const;

  void operator()(code_formatter& formatter, const ast::optional_output_branch& x) const;

  // Accept ast::variant and delegate formatting of the stored type to our derived class.
  // Using enable_if here to prevent implicit conversion to the variant type.
  template <typename T, typename = std::enable_if_t<std::is_same_v<T, ast::variant>>>
  void operator()(code_formatter& formatter, const T& var) const {
    std::visit([&](const auto& x) { return operator()(formatter, x); }, var);
  }

  // Accept ast::variant_ptr
  void operator()(code_formatter& formatter, const ast::variant_ptr& var) const {
    WF_ASSERT(var);
    operator()(formatter, *var);
  }

 private:
  void format_signature(code_formatter& formatter, const ast::function_signature& signature) const;
};

}  // namespace wf
