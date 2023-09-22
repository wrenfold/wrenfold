// Copyright 2023 Gareth Cross
#include "cpp_code_generator.h"

#include "code_generation/ast_formatters.h"

namespace math {

static constexpr std::string_view UtilityNamespace = "math";

std::string CppCodeGenerator::Generate(const ast::FunctionDefinition& definition) const {
  CodeFormatter result;
  FormatSignature(result, definition.signature);
  result.WithIndentation(2, "{\n", "\n}", [&] {
    //
    std::size_t counter = 0;
    for (const auto& arg : definition.signature.arguments) {
      if (arg->IsMatrix()) {
        const ast::MatrixType& mat = std::get<ast::MatrixType>(arg->Type());

        // Generate matrix conversion logic.
        // TODO: Support dynamic sizes here too.
        result.Format("auto _{} = ", arg->Name());
        const std::string dims_type = fmt::format("{}, {}", mat.NumRows(), mat.NumCols());
        switch (arg->Direction()) {
          case ast::ArgumentDirection::Input:
            result.Format("{}::make_input_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->Name());
            break;
          case ast::ArgumentDirection::Output:
            result.Format("{}::make_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->Name());
            break;
          case ast::ArgumentDirection::OptionalOutput:
            result.Format("{}::make_optional_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->Name());
            break;
        }
        ++counter;
      }
    }

    if (counter > 0) {
      result.Append("\n");
    }

    result.Join(*this, "\n", definition.body);
  });
  return result.GetOutput();
}

constexpr static std::string_view StringFromNumericCastType(const NumericType destination_type) {
  switch (destination_type) {
    case NumericType::Bool:
      return "bool";
    case NumericType::Integer:
      return "std::int64_t";
    case NumericType::Real:
      return "Scalar";
    case NumericType::Complex:
      return "std::complex<Scalar>";
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::FormatSignature(CodeFormatter& formatter,
                                       const ast::FunctionSignature& signature) const {
  formatter.Format("template <typename Scalar");
  const bool has_matrix_args =
      std::any_of(signature.arguments.begin(), signature.arguments.end(),
                  [](const std::shared_ptr<const ast::Argument>& arg) { return arg->IsMatrix(); });

  if (has_matrix_args) {
    std::size_t counter = 0;
    for (const std::shared_ptr<const ast::Argument>& arg : signature.arguments) {
      if (arg->IsMatrix()) {
        formatter.Format(", typename T{}", counter);
        ++counter;
      }
    }
  }
  formatter.Append(">\n");

  if (signature.return_value) {
    if (!std::holds_alternative<ast::ScalarType>(*signature.return_value)) {
      // TODO: To support returning matrices in C++ we need more than just a `span` type.
      throw TypeError("Only scalars can be returned.");
    } else {
      formatter.Format("auto {}(", signature.function_name);
    }
  } else {
    formatter.Format("void {}(", signature.function_name);
  }

  std::size_t counter = 0;
  auto arg_printer = [&counter](CodeFormatter& formatter, const ast::Argument::shared_ptr& arg) {
    if (arg->IsMatrix()) {
      if (arg->Direction() == ast::ArgumentDirection::Input) {
        formatter.Format("const T{}&", counter);
      } else {
        formatter.Format("T{}&&", counter);
      }
      ++counter;
    } else {
      if (arg->Direction() == ast::ArgumentDirection::Input) {
        formatter.Append("const ");
      }
      const NumericType numeric_type = std::get<ast::ScalarType>(arg->Type()).GetNumericType();
      formatter.Append(StringFromNumericCastType(numeric_type));
    }

    formatter.Format(" {}", arg->Name());
  };

  formatter.Join(std::move(arg_printer), ", ", signature.arguments);
  formatter.Append(")\n");
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::ScalarType& scalar,
                                  const TypeContext context) const {
  const std::string_view numeric_type = StringFromNumericCastType(scalar.GetNumericType());
  switch (context) {
    case TypeContext::InputArgument:
    case TypeContext::FunctionBody:
    case TypeContext::ReturnValue: {
      formatter.Format(numeric_type);
      break;
    }
    case TypeContext::OutputArgument: {
      formatter.Format("{}&", numeric_type);
      break;
    }
    case TypeContext::OptionalOutputArgument: {
      formatter.Format("{}*", numeric_type);
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
  const auto& dest_name = assignment.argument->Name();
  const ast::Type& type = assignment.argument->Type();

  if (std::holds_alternative<ast::MatrixType>(type)) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(type);
    auto range = MakeRange<std::size_t>(0, assignment.values.size());

    // TODO: If there is a unit dimension, use the [] operator?
    formatter.Join(
        [&](CodeFormatter& fmt, std::size_t i) {
          const auto [row, col] = mat.ComputeIndices(i);
          fmt.Format("{}{}({}, {}) = {};", assignment.argument->IsMatrix() ? "_" : "", dest_name,
                     row, col, View(assignment.values[i]));
        },
        "\n", range);

  } else {
    // Otherwise it is a scalar, so just assign it:
    ASSERT_EQUAL(1, assignment.values.size());
    formatter.Format("{} = {};", dest_name, assignment.values.front());
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.Format("{} = {};", x.left, View(x.right));
}

static inline std::string_view GetBuiltInFunctionCall(const BuiltInFunctionName name) {
  switch (name) {
    case BuiltInFunctionName::Cos:
      return "std::cos";
    case BuiltInFunctionName::Sin:
      return "std::sin";
    case BuiltInFunctionName::Tan:
      return "std::tan";
    case BuiltInFunctionName::ArcCos:
      return "std::acos";
    case BuiltInFunctionName::ArcSin:
      return "std::asin";
    case BuiltInFunctionName::ArcTan:
      return "std::atan";
    case BuiltInFunctionName::Log:
      return "std::log";
    case BuiltInFunctionName::Sqrt:
      return "std::sqrt";
    case BuiltInFunctionName::Arctan2:
      return "std::atan2";
    case BuiltInFunctionName::Pow:
      return "std::pow";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Call& x) const {
  return formatter.Format("{}({})", GetBuiltInFunctionCall(x.function), Join(*this, ", ", x.args));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  formatter.Format("static_cast<{}>({})", StringFromNumericCastType(x.destination_type),
                   View(x.arg));
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
  ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  ASSERT_EQUAL(1, x.args.size());
  formatter.Format("return {};", View(x.args[0]));
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
    const auto [r, c] = mat.ComputeIndices(x.element);
    formatter.Format("_{}({}, {})", x.argument->Name(), r, c);
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.Format("{} * {}", View(x.left), View(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::OutputExists& x) const {
  formatter.Format("static_cast<bool>({}{})", x.argument->IsMatrix() ? "_" : "",
                   x.argument->Name());
}

}  // namespace math
