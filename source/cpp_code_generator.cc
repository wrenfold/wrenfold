// Copyright 2023 Gareth Cross
#include "cpp_code_generator.h"

namespace math {

void CppCodeGenerator::Generate(CodeFormatter& formatter, const FunctionDescription& func,
                                const std::vector<ast::Variant>& ast) {
  PutFunctionSignature(formatter, func);
  formatter.WithIndentation(2, "{\n", "}", [this, &ast](CodeFormatter& formatter) {
    for (const ast::Variant& var : ast) {
      Format(formatter, var);
      formatter.Append("\n");
    }
  });
}

void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::ScalarType&,
                              TypeContext context) const {
  switch (context) {
    case TypeContext::InputArgument:
    case TypeContext::FunctionBody:
    case TypeContext::ReturnValue: {
      formatter.Append("double");
      break;
    }
    case TypeContext::OutputArgument: {
      formatter.Append("double&");
      break;
    }
    case TypeContext::OptionalOutputArgument: {
      formatter.Append("double*");
      break;
    }
  }
}

void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::MatrixType& mat,
                              TypeContext context) const {
  switch (context) {
    case TypeContext::InputArgument: {
      formatter.Format("Eigen::Matrix<double, {}, {}>&", mat.NumRows(), mat.NumCols());
      break;
    }
    case TypeContext::FunctionBody:
    case TypeContext::ReturnValue: {
      formatter.Format("Eigen::Matrix<double, {}, {}>", mat.NumRows(), mat.NumCols());
      break;
    }
    case TypeContext::OutputArgument: {
      formatter.Format("Eigen::Ref<Eigen::Matrix<double, {}, {}>>", mat.NumRows(), mat.NumCols());
      break;
    }
    case TypeContext::OptionalOutputArgument: {
      formatter.Format("std::optional<Eigen::Ref<Eigen::Matrix<double, {}, {}>>>", mat.NumRows(),
                       mat.NumCols());
      break;
    }
  }
}

void CppCodeGenerator::FormatReturnType(CodeFormatter& formatter,
                                        const FunctionDescription& description) const {
  //  formatter.Append("inline ");
  if (description.return_values.empty()) {
    formatter.Append("void");
  } else if (description.return_values.size() == 1) {
    // there is a single type
    Format(formatter, description.return_values.front(), TypeContext::ReturnValue);
  } else {
    // make a tuple
    formatter.Append("std::tuple<");
    for (const auto& type : description.return_values) {
      Format(formatter, type, TypeContext::ReturnValue);
      formatter.Append(", ");
    }
    formatter.RightTrimTrailingWhitespaceAndComma();
    formatter.Append(">");
  }
}

void CppCodeGenerator::PutFunctionSignature(CodeFormatter& formatter,
                                            const FunctionDescription& description) {
  FormatReturnType(formatter, description);
  formatter.Format(" {}(", description.function_name);
  for (const std::shared_ptr<const ast::Argument>& input_arg : description.input_args) {
    formatter.Append("const ");
    Format(formatter, input_arg->Type(), TypeContext::InputArgument);
    formatter.Format(" {}, ", input_arg->Name());
  }
  for (const std::shared_ptr<const ast::Argument>& output_arg : description.output_args) {
    Format(formatter, output_arg->Type(),
           output_arg->IsOptional() ? TypeContext::OptionalOutputArgument
                                    : TypeContext::OutputArgument);
    formatter.Format(" {}, ", output_arg->Name());
  }
  formatter.RightTrimTrailingWhitespaceAndComma();
  formatter.Append(")\n");
}

inline std::string_view GetUnaryFunctionCall(const UnaryFunctionName name) {
  switch (name) {
    case UnaryFunctionName::Cos:
      return "std::cos";
    case UnaryFunctionName::Sin:
      return "std::sin";
    case UnaryFunctionName::Tan:
      return "std::tan";
    case UnaryFunctionName::ArcCos:
      return "std::acos";
    case UnaryFunctionName::ArcSin:
      return "std::asin";
    case UnaryFunctionName::ArcTan:
      return "std::atan";
    case UnaryFunctionName::Log:
      return "std::log";
    case UnaryFunctionName::Sqrt:
      return "std::sqrt";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

inline std::string_view GetBinaryFunctionCall(const BinaryFunctionName name) {
  switch (name) {
    case BinaryFunctionName::Mod:
      return "std::fmod";
    case BinaryFunctionName::Pow:
      return "std::pow";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::Add& x) const {
  formatter.FormatWith(*this, "{} + {}", x.left, x.right);
}
void CppCodeGenerator::Format(CodeFormatter&, const ast::ArrayAccess&) const { ASSERT(false); }
void CppCodeGenerator::Format(CodeFormatter&, const ast::Assignment&) const { ASSERT(false); }
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::Call& x) const {
  if (std::holds_alternative<UnaryFunctionName>(x.function)) {
    formatter.FormatWith(*this, "{}({})",
                         GetUnaryFunctionCall(std::get<UnaryFunctionName>(x.function)),
                         x.args.front());
  } else if (std::holds_alternative<BinaryFunctionName>(x.function)) {
    formatter.FormatWith(*this, "{}({}, {})",
                         GetBinaryFunctionCall(std::get<BinaryFunctionName>(x.function)),
                         x.args.front(), x.args.back());
  } else {
    ASSERT(false);
  }
}
void CppCodeGenerator::Format(CodeFormatter&, const ast::Cast&) const { ASSERT(false); }
void CppCodeGenerator::Format(CodeFormatter&, const ast::Conditional&) const { ASSERT(false); }
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.FormatWith(*this, "{} {};", x.type, x.name);
  } else {
    formatter.FormatWith(*this, "const {} {} = {};", x.type, x.name, x.value);
  }
}
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::FloatConstant& x) const {
  formatter.Format("{:.16}", x.value);
}
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::InputValue& x) const {
  ASSERT(x.argument);
  if (std::holds_alternative<ast::ScalarType>(x.argument->Type())) {
    formatter.Append(x.argument->Name());
  } else {
    const ast::MatrixType& mat = std::get<ast::MatrixType>(x.argument->Type());
    if (mat.NumCols() == 1) {
      formatter.Format("{}[{}]", x.argument->Name(), x.element);
    } else {
      const auto [r, c] = mat.ComputeIndices(x.element);
      formatter.Format("{}({}, {})", x.argument->Name(), r, c);
    }
  }
}
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::IntegerConstant& x) const {
  formatter.Format("{}", x.value);
}
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.FormatWith(*this, "{} * {}", x.left, x.right);
}
void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::OutputBlock& x) const {
  ASSERT(x.argument);
  const ast::Argument& arg = *x.argument;
  const ast::Type& arg_type = arg.Type();

  if (x.output_type == OutputType::OutputArgument) {
    formatter.Format("// Generate output argument: {}\n", arg.Name());
    formatter.WithIndentation(2, "{\n", "}", [this, &x, arg, arg_type](CodeFormatter& formatter) {
      for (const ast::Variant& statement : x.statements) {
        Format(formatter, statement);
        formatter.Append("\n");
      }
      if (std::holds_alternative<ast::ScalarType>(arg_type)) {
        ASSERT_EQUAL(x.outputs.size(), 1);
        formatter.Format("{} = {};\n", arg.Name(), x.outputs.front().name);
      } else {
        const ast::MatrixType& mat = std::get<ast::MatrixType>(arg_type);
        // insert output statements
        for (std::size_t i = 0; i < Dimension(arg_type); ++i) {
          const auto [row, col] = mat.ComputeIndices(i);
          formatter.Format("{}({}, {}) = {};\n", arg.Name(), row, col, x.outputs[i].name);
        }
      }
    });
  } else if (x.output_type == OutputType::OptionalOutputArgument) {
    formatter.Format("// Generate optional output argument: {}\n", arg.Name());
    formatter.Format("if (static_cast<bool>({})) ", arg.Name());
    formatter.WithIndentation(2, "{\n", "}", [this, &x, arg, arg_type](CodeFormatter& formatter) {
      Format(formatter, arg.Type(), TypeContext::OutputArgument);
      formatter.Format("& _{} = *{};\n", arg.Name(), arg.Name());

      if (std::holds_alternative<ast::ScalarType>(arg_type)) {
        ASSERT_EQUAL(x.outputs.size(), 1);
        formatter.Format("_{} = {};\n", arg.Name(), x.outputs.front().name);
      } else {
        const ast::MatrixType& mat = std::get<ast::MatrixType>(arg_type);
        // insert output statements
        for (std::size_t i = 0; i < Dimension(arg_type); ++i) {
          const auto [row, col] = mat.ComputeIndices(i);
          formatter.Format("_{}({}, {}) = {};\n", arg.Name(), row, col, x.outputs[i].name);
        }
      }
    });
  }
}

void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::ReturnValueBlock& retval) const {
  for (std::size_t i = 0; i < retval.values.size(); ++i) {
    const ast::ReturnValueBlock::ReturnValue& val = retval.values[i];
    Format(formatter, val.type, TypeContext::ReturnValue);

    if (std::holds_alternative<ast::ScalarType>(val.type)) {
      ASSERT_EQUAL(val.outputs.size(), 1);
      formatter.Format(" _return_value_{:02} = {};\n", i, val.outputs.front().name);
    } else {
      ASSERT_EQUAL(val.outputs.size(), Dimension(val.type));
      formatter.Format(" _return_value_{:02};\n", i);
      const ast::MatrixType& mat = std::get<ast::MatrixType>(val.type);
      // insert output statements
      for (std::size_t element = 0; element < mat.Dimension(); ++element) {
        const auto [row, col] = mat.ComputeIndices(element);
        formatter.Format("_return_value_{:02}({}, {}) = {};\n", i, row, col,
                         val.outputs[element].name);
      }
    }
  }

  // create the return statement itself
  if (retval.values.size() > 1) {
    formatter.Append("return std::make_tuple(");
    for (std::size_t i = 0; i < retval.values.size(); ++i) {
      formatter.Format("_return_value_{:02}, ", i);
    }
    formatter.RightTrimTrailingWhitespaceAndComma();
    formatter.Append(");");
  } else if (retval.values.size() == 1) {
    formatter.Format("return _return_value_{:02};", 0);
  }
}

void CppCodeGenerator::Format(CodeFormatter& formatter, const ast::VariableRef& x) const {
  formatter.Append(x.name);
}

}  // namespace math
