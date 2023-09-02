// Copyright 2023 Gareth Cross
#pragma once
#include "code_formatter.h"

namespace math {

class CppCodeGenerator {
 public:
  enum class TypeContext {
    FunctionBody,
    ReturnValue,
    InputArgument,
    OutputArgument,
    OptionalOutputArgument,
  };

  std::string Generate(const ast::FunctionDefinition& func) const;

  // Create a FmtView that can be passed to CodeFormatter. All args will be
  // forwarded back to the operator on this class that matches them.
  template <typename... Args>
  auto View(Args&&... args) const {
    return ::math::View(*this, std::forward<Args>(args)...);
  }

  void operator()(CodeFormatter& formatter, const ast::ScalarType&,
                  const TypeContext context) const;

  void operator()(CodeFormatter& formatter, const ast::MatrixType& mat,
                  const TypeContext context) const;

  void operator()(CodeFormatter& formatter, const ast::Type& x, TypeContext context) const {
    std::visit([&](const auto& type) { return operator()(formatter, type, context); }, x);
  }

  void operator()(CodeFormatter& formatter, const ast::Add& x) const;

  void operator()(CodeFormatter& formatter, const ast::AssignOutputArgument& x) const;

  void operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const;

  void operator()(CodeFormatter& formatter, const ast::Branch& x) const;

  void operator()(CodeFormatter& formatter, const ast::Call& x) const;

  void operator()(CodeFormatter& formatter, const ast::Cast& x) const;

  void operator()(CodeFormatter& formatter, const ast::Compare& x) const;

  void operator()(CodeFormatter& formatter, const ast::ConstructReturnValue& x) const;

  void operator()(CodeFormatter& formatter, const ast::Declaration& x) const;

  void operator()(CodeFormatter& formatter, const ast::FloatConstant& x) const {
    formatter.Format("{:.16}", x.value);
  }

  void operator()(CodeFormatter& formatter, const ast::VariableRef& x) const {
    formatter.Append(x.name);
  }

  void operator()(CodeFormatter& formatter, const ast::InputValue& x) const;

  void operator()(CodeFormatter& formatter, const ast::IntegerConstant& x) const {
    formatter.Format("{}", x.value);
  }

  void operator()(CodeFormatter& formatter, const ast::Multiply& x) const;

  void operator()(CodeFormatter& formatter, const ast::OutputExists& x) const;

  void operator()(CodeFormatter& formatter, const ast::ReturnValue& v) const;

  // Accept ast::Variant and delegate formatting of the stored type to our derived class.
  // Using enable_if here to prevent implicit conversion to the variant type.
  template <typename T, typename = std::enable_if_t<std::is_same_v<T, ast::Variant>>>
  void operator()(CodeFormatter& formatter, const T& var) const {
    std::visit([&](const auto& x) { return operator()(formatter, x); }, var);
  }

  // Accept ast::VariantPtr
  void operator()(CodeFormatter& formatter, const ast::VariantPtr& var) const {
    ASSERT(var);
    operator()(formatter, *var);
  }

  // Format ptr to argument.
  void operator()(CodeFormatter& formatter, const std::shared_ptr<const ast::Argument>& var) const {
    ASSERT(var);
    formatter.Append(var->Name());
  }

 protected:
  void FormatReturnType(CodeFormatter& formatter, const ast::FunctionSignature& signature) const;
  void FormatSignature(CodeFormatter& formatter, const ast::FunctionSignature& signature) const;
};

}  // namespace math
