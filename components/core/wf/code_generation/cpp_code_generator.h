// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/code_formatter.h"

namespace math {

class cpp_code_generator {
 public:
  std::string generate_code(const ast::FunctionSignature& signature,
                            const std::vector<ast::Variant>& body) const;

  // Create a fmt_view that can be passed to code_formatter. All args will be
  // forwarded back to the operator on this class that matches them.
  template <typename... Args>
  auto make_view(Args&&... args) const {
    return ::math::make_fmt_view(*this, std::forward<Args>(args)...);
  }

  void operator()(code_formatter& formatter, const ast::Add& x) const;

  void operator()(code_formatter& formatter, const ast::AssignOutputArgument& x) const;

  void operator()(code_formatter& formatter, const ast::AssignTemporary& x) const;

  void operator()(code_formatter& formatter, const ast::Branch& x) const;

  void operator()(code_formatter& formatter, const ast::Call& x) const;

  void operator()(code_formatter& formatter, const ast::Cast& x) const;

  void operator()(code_formatter& formatter, const ast::Compare& x) const;

  void operator()(code_formatter& formatter, const ast::ConstructReturnValue& x) const;

  void operator()(code_formatter& formatter, const ast::Declaration& x) const;

  void operator()(code_formatter& formatter, const ast::Divide& x) const;

  void operator()(code_formatter& formatter, const ast::FloatConstant& x) const {
    formatter.format("static_cast<Scalar>({})", x.value);
  }

  void operator()(code_formatter& formatter, const ast::SpecialConstant& x) const;

  void operator()(code_formatter& formatter, const ast::VariableRef& x) const {
    formatter.format(x.name);
  }

  void operator()(code_formatter& formatter, const ast::InputValue& x) const;

  void operator()(code_formatter& formatter, const ast::IntegerConstant& x) const {
    formatter.format("{}", x.value);
  }

  void operator()(code_formatter& formatter, const ast::Multiply& x) const;

  void operator()(code_formatter& formatter, const ast::OptionalOutputBranch& x) const;

  // Accept ast::Variant and delegate formatting of the stored type to our derived class.
  // Using enable_if here to prevent implicit conversion to the variant type.
  template <typename T, typename = std::enable_if_t<std::is_same_v<T, ast::Variant>>>
  void operator()(code_formatter& formatter, const T& var) const {
    std::visit([&](const auto& x) { return operator()(formatter, x); }, var);
  }

  // Accept ast::VariantPtr
  void operator()(code_formatter& formatter, const ast::VariantPtr& var) const {
    WF_ASSERT(var);
    operator()(formatter, *var);
  }

  // Format ptr to argument.
  void operator()(code_formatter& formatter,
                  const std::shared_ptr<const ast::argument>& var) const {
    WF_ASSERT(var);
    formatter.format(var->name());
  }

 protected:
  void format_signature(code_formatter& formatter, const ast::FunctionSignature& signature) const;
};

}  // namespace math
