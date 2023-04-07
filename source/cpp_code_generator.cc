// Copyright 2023 Gareth Cross
#include "cpp_code_generator.h"

namespace math {

std::string CppCodeGenerator::Generate(const ast::FunctionDefinition& definition) const {
  CodeFormatter result;
  FormatSignature(result, definition.signature);
  result.WithIndentation(2, "{\n", "\n}", [&] { result.Join(*this, "\n", definition.body); });
  return result.GetOutput();
}

void CppCodeGenerator::FormatReturnType(CodeFormatter& formatter,
                                        const ast::FunctionSignature& signature) const {
  if (signature.return_values.empty()) {
    formatter.Append("void");
  } else if (signature.return_values.size() == 1) {
    // there is a single type
    operator()(formatter, signature.return_values[0], TypeContext::ReturnValue);
  } else {
    // make a tuple
    formatter.Append("std::tuple<");
    formatter.Join(
        [&](CodeFormatter& formatter, const ast::Type& type) {
          operator()(formatter, type, TypeContext::ReturnValue);
        },
        ", ", signature.return_values);
    formatter.Append(">");
  }
}

void CppCodeGenerator::FormatSignature(CodeFormatter& formatter,
                                       const ast::FunctionSignature& signature) const {
  FormatReturnType(formatter, signature);
  formatter.Format(" {}(", signature.function_name);

  auto input_arg_printer = [this](CodeFormatter& formatter, const ast::Argument::shared_ptr& arg) {
    formatter.Format("const {} {}", View(arg->Type(), TypeContext::InputArgument), arg->Name());
  };
  formatter.Join(std::move(input_arg_printer), ", ", signature.input_args);
  if (!signature.output_args.empty()) {
    formatter.Append(", ");
  }

  auto output_arg_printer = [this](CodeFormatter& formatter, const ast::Argument::shared_ptr& arg) {
    const auto arg_context =
        arg->IsOptional() ? TypeContext::OptionalOutputArgument : TypeContext::OutputArgument;
    formatter.Format("{} {}", View(arg->Type(), arg_context), arg->Name());
  };
  formatter.Join(std::move(output_arg_printer), ", ", signature.output_args);
  formatter.Append(")\n");
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::ScalarType&,
                                  const TypeContext context) const {
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

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::MatrixType& mat,
                                  const TypeContext context) const {
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

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Add& x) const {
  formatter.Format("{} + {}", View(x.left), View(x.right));
}

void CppCodeGenerator::FormatOutputArg(CodeFormatter& formatter, const ast::Argument& arg,
                                       const ast::Variant& right) const {
  if (std::holds_alternative<ast::ConstructMatrix>(right)) {
    // For optional args, we need to de-reference to get the underlying Eigen::Ref
    std::string dest_name = arg.Name();
    if (arg.IsOptional()) {
      dest_name = fmt::format("_{}", arg.Name());
      formatter.Format("{}& {} = *{};\n", View(arg.Type(), TypeContext::OutputArgument), dest_name,
                       arg.Name());
    }

    // Now write to the output variable:
    const ast::ConstructMatrix& mat_values = std::get<ast::ConstructMatrix>(right);

    auto range = MakeRange<std::size_t>(0, mat_values.args.size());
    if (mat_values.type.HasUnitDimension()) {
      formatter.Join(
          [&](CodeFormatter& fmt, std::size_t i) {
            fmt.Format("{}[{}] = {};", dest_name, i, View(mat_values.args[i]));
          },
          "\n", range);
    } else {
      formatter.Join(
          [&](CodeFormatter& fmt, std::size_t i) {
            const auto [row, col] = mat_values.type.ComputeIndices(i);
            fmt.Format("[]({}, {}) = {};", dest_name, row, col, View(mat_values.args[i]));
          },
          "\n", range);
    }
  } else {
    ASSERT_EQUAL(1, arg.TypeDimension());  // TODO: Needs relaxing for custom types.
    formatter.Format("{}{} = {};", arg.IsOptional() ? "*" : "", arg.Name(), View(right));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Assignment& x) const {
  std::visit(
      [&](const auto& left) {
        using T = std::decay_t<decltype(left)>;
        if constexpr (std::is_same_v<T, std::shared_ptr<const ast::Argument>>) {
          FormatOutputArg(formatter, *left, *x.right);
        } else {
          formatter.Format("{} = {};", View(left), View(x.right));
        }
      },
      x.left);
}

static inline std::string_view GetUnaryFunctionCall(const UnaryFunctionName name) {
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

static inline std::string_view GetBinaryFunctionCall(const BinaryFunctionName name) {
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

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Call& x) const {
  if (std::holds_alternative<UnaryFunctionName>(x.function)) {
    return formatter.Format("{}({})", GetUnaryFunctionCall(std::get<UnaryFunctionName>(x.function)),
                            View(x.args.front()));
  } else if (std::holds_alternative<BinaryFunctionName>(x.function)) {
    return formatter.Format("{}({}, {})",
                            GetBinaryFunctionCall(std::get<BinaryFunctionName>(x.function)),
                            View(x.args.front()), View(x.args.back()));
  }
  ASSERT(false, "Invalid function type. TODO: Implement me");
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Conditional& x) const {
  formatter.Format("if ({}) ", View(x.condition));
  formatter.WithIndentation(2, "{\n", "\n}", [&] { formatter.Join(*this, "\n", x.if_branch); });
  if (!x.else_branch.empty()) {
    formatter.WithIndentation(2, "{\n", "\n}", [&] { formatter.Join(*this, "\n", x.else_branch); });
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::ConstructMatrix& x) const {
  formatter.Format("{}", View(x.type, TypeContext::ReturnValue));
  if (x.type.HasUnitDimension()) {
    formatter.Append("{");
    formatter.Join(*this, ", ", x.args);
    formatter.Append("}");
  } else {
    formatter.WithIndentation(2, "{\n", "}", [&] {
      formatter.Join(
          [&](CodeFormatter& formatter, index_t row) {
            formatter.Append("{");
            formatter.Join(
                [&](CodeFormatter& formatter, index_t col) {
                  this->operator()(formatter, x.args[row * x.type.NumCols() + col]);
                },
                ", ", MakeRange(0, x.type.NumCols()));
            formatter.Append("}");
          },
          ",\n", MakeRange(0, x.type.NumRows()));
    });
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.Format("{} {};", View(x.type, TypeContext::FunctionBody), x.name);
  } else {
    formatter.Format("const {} {} = {};", View(x.type, TypeContext::FunctionBody), x.name,
                     View(x.value));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
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

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.Format("{} * {}", View(x.left), View(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::OutputExists& x) const {
  formatter.Format("static_cast<bool>({})", x.argument->Name());
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::ReturnValue& v) const {
  if (v.values.empty()) {
    // No return statement.
  } else if (v.values.size() == 1) {
    formatter.Format("return {};", View(v.values[0]));
  } else {
    formatter.Append("std::make_tuple(");
    formatter.Join(*this, ", ", v.values);
    formatter.Append(");");
  }
}

}  // namespace math
