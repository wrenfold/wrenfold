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

  void Generate(CodeFormatter& formatter, const FunctionDescription& func,
                const std::vector<ast::Variant>& ast);

  void Format(CodeFormatter& formatter, const ast::Type& type,
              TypeContext context = TypeContext::FunctionBody) const {
    std::visit([this, &formatter, context](const auto& type) { Format(formatter, type, context); },
               type);
  }

  void Format(CodeFormatter& formatter, const ast::ScalarType&, TypeContext context) const;
  void Format(CodeFormatter& formatter, const ast::MatrixType& mat, TypeContext context) const;

  void FormatReturnType(CodeFormatter& formatter, const FunctionDescription& description) const;

  void PutFunctionSignature(CodeFormatter& formatter, const FunctionDescription& description);

  void Format(CodeFormatter& formatter, const ast::Add& x) const;
  void Format(CodeFormatter& formatter, const ast::ArrayAccess& x) const;
  void Format(CodeFormatter& formatter, const ast::Assignment& x) const;
  void Format(CodeFormatter& formatter, const ast::Call& x) const;
  void Format(CodeFormatter& formatter, const ast::Cast& x) const;
  void Format(CodeFormatter& formatter, const ast::Conditional& x) const;
  void Format(CodeFormatter& formatter, const ast::Declaration& x) const;
  void Format(CodeFormatter& formatter, const ast::FloatConstant& x) const;
  void Format(CodeFormatter& formatter, const ast::InputValue& x) const;
  void Format(CodeFormatter& formatter, const ast::IntegerConstant& x) const;
  void Format(CodeFormatter& formatter, const ast::Multiply& x) const;
  void Format(CodeFormatter& formatter, const ast::OutputBlock& x) const;
  void Format(CodeFormatter& formatter, const ast::ReturnValueBlock& x) const;
  void Format(CodeFormatter& formatter, const ast::VariableRef& x) const;

  void Format(CodeFormatter& formatter, const ast::Variant& var) const {
    std::visit([this, &formatter](const auto& x) { this->Format(formatter, x); }, var);
  }
  void Format(CodeFormatter& formatter, const ast::VariantPtr& var_ptr) const {
    ASSERT(var_ptr);
    Format(formatter, *var_ptr);
  }
};

}  // namespace math
