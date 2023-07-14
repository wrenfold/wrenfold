// Copyright 2023 Gareth Cross
#include "cpp_code_generator.h"

namespace math {

std::string CppCodeGenerator::Generate(const ast::FunctionDefinition& definition) const {
  //
  for (const auto& thing : definition.body) {
    fmt::print("{}\n", thing);
  }

  CodeFormatter result;
  FormatSignature(result, definition.signature);
  result.WithIndentation(2, "{\n", "\n}", [&] {
    result.Join(*this, "\n", definition.body);
    result.Append("\n");
    const std::size_t num_return_vals = definition.signature.return_values.size();
    if (num_return_vals == 0) {
      // No return statement.
    } else if (num_return_vals == 1) {
      result.Append("return return_value_0;");
    } else {
      result.Append("return std::make_tuple(");
      result.Join([&](CodeFormatter& formatter,
                      const std::size_t index) { formatter.Format("return_value_{}", index); },
                  ", ", MakeRange<std::size_t>(0, num_return_vals));
      result.Append(");");
    }
  });
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

CppCodeGenerator::TypeContext TypeContextFromArgumentDirection(ast::ArgumentDirection dir) {
  switch (dir) {
    case ast::ArgumentDirection::Input:
      return CppCodeGenerator::TypeContext::InputArgument;
    case ast::ArgumentDirection::Output:
      return CppCodeGenerator::TypeContext::OutputArgument;
    case ast::ArgumentDirection::OptionalOutput:
      return CppCodeGenerator::TypeContext::OptionalOutputArgument;
  }
  throw AssertionError("Invalid argument direction specified");
}

void CppCodeGenerator::FormatSignature(CodeFormatter& formatter,
                                       const ast::FunctionSignature& signature) const {
  FormatReturnType(formatter, signature);
  formatter.Format(" {}(", signature.function_name);

  auto arg_printer = [this](CodeFormatter& formatter, const ast::Argument::shared_ptr& arg) {
    const auto type_context = TypeContextFromArgumentDirection(arg->Direction());
    if (arg->Direction() == ast::ArgumentDirection::Input) {
      formatter.Append("const ");
    }
    formatter.Format("{} {}", View(arg->Type(), type_context), arg->Name());
  };
  formatter.Join(std::move(arg_printer), ", ", signature.arguments);
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

void CppCodeGenerator::operator()(CodeFormatter& formatter,
                                  const ast::AssignOutputArgument& assignment) const {
  // For optional args, we need to de-reference to get the underlying Eigen::Ref
  std::string dest_name = assignment.argument->Name();
  const ast::Type& type = assignment.argument->Type();
  if (assignment.argument->IsOptional()) {
    dest_name = fmt::format("_{}", dest_name);
    formatter.Format("{}& {} = *{};\n", View(type, TypeContext::FunctionBody), dest_name,
                     assignment.argument->Name());
  }

  if (std::holds_alternative<ast::MatrixType>(type)) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(type);
    auto range = MakeRange<std::size_t>(0, assignment.values.size());

    if (mat.HasUnitDimension()) {
      formatter.Join(
          [&](CodeFormatter& fmt, std::size_t i) {
            fmt.Format("{}[{}] = {};", dest_name, i, View(assignment.values[i]));
          },
          "\n", range);
    } else {
      formatter.Join(
          [&](CodeFormatter& fmt, std::size_t i) {
            const auto [row, col] = mat.ComputeIndices(i);
            fmt.Format("[]({}, {}) = {};", dest_name, row, col, View(assignment.values[i]));
          },
          "\n", range);
    }
  } else {
    // Otherwise it is a scalar, so just assign it:
    ASSERT_EQUAL(1, assignment.values.size());
    formatter.Format("{} = {};", dest_name, assignment.values.front());
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.Format("{} = {};", x.left, View(x.right));
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

constexpr std::string_view GetCastType(const NumericType destination_type) {
  switch (destination_type) {
    case NumericType::Bool:
      return "bool";
    case NumericType::Integer:
      return "std::int64_t";
    case NumericType::Real:
      return "double";
    case NumericType::Complex:
      return "std::complex<double>";
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  formatter.Format("static_cast<{}>({})", GetCastType(x.destination_type), View(x.arg));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Compare& x) const {
  formatter.Format("{} {} {}", View(x.left), StringFromRelationalOperation(x.operation),
                   View(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Branch& x) const {
  formatter.Format("if ({}) ", View(x.condition));
  formatter.WithIndentation(2, "{\n", "\n}", [&] { formatter.Join(*this, "\n", x.if_branch); });
  if (!x.else_branch.empty()) {
    formatter.WithIndentation(2, " else {\n", "\n}",
                              [&] { formatter.Join(*this, "\n", x.else_branch); });
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter,
                                  const ast::ConstructReturnValue& x) const {
  formatter.Format("{} return_value_{}", View(x.type, TypeContext::ReturnValue), x.position);

  if (std::holds_alternative<ast::ScalarType>(x.type)) {
    ASSERT_EQUAL(1, x.args.size());
    formatter.Format(" = {}", View(x.args.front()));
  } else {
    const ast::MatrixType& type = std::get<ast::MatrixType>(x.type);
    if (type.HasUnitDimension()) {
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
                    this->operator()(formatter, x.args[row * type.NumCols() + col]);
                  },
                  ", ", MakeRange(0, type.NumCols()));
              formatter.Append("}");
            },
            ",\n", MakeRange(0, type.NumRows()));
      });
    }
  }
  formatter.Append(";");
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
