// Copyright 2023 Gareth Cross
#include "code_generation/cpp_code_generator.h"

#include "code_generation/ast_formatters.h"

namespace math {

static constexpr std::string_view UtilityNamespace = "math";

std::string CppCodeGenerator::generate_code(const ast::FunctionSignature& signature,
                                            const std::vector<ast::Variant>& body) const {
  CodeFormatter result;
  format_signature(result, signature);
  result.with_indentation(2, "{\n", "\n}", [&] {
    // Convert input args to spans:
    std::size_t counter = 0;
    for (const auto& arg : signature.arguments) {
      if (arg->is_matrix()) {
        const ast::MatrixType& mat = std::get<ast::MatrixType>(arg->type());

        // Generate matrix conversion logic.
        // TODO: Support dynamic sizes here too.
        result.format("auto _{} = ", arg->name());
        const std::string dims_type = fmt::format("{}, {}", mat.rows(), mat.cols());
        switch (arg->direction()) {
          case ast::ArgumentDirection::Input:
            result.format("{}::make_input_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
          case ast::ArgumentDirection::Output:
            result.format("{}::make_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
          case ast::ArgumentDirection::OptionalOutput:
            result.format("{}::make_optional_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
        }
        ++counter;
      }
    }

    if (counter > 0) {
      result.format("\n");
    }

    result.join(*this, "\n", body);
  });
  return result.get_output();
}

constexpr static std::string_view string_from_numeric_cast_type(
    const NumericType destination_type) {
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

void CppCodeGenerator::format_signature(CodeFormatter& formatter,
                                        const ast::FunctionSignature& signature) const {
  formatter.format("template <typename Scalar");
  const bool has_matrix_args =
      std::any_of(signature.arguments.begin(), signature.arguments.end(),
                  [](const std::shared_ptr<const ast::Argument>& arg) { return arg->is_matrix(); });

  if (has_matrix_args) {
    std::size_t counter = 0;
    for (const std::shared_ptr<const ast::Argument>& arg : signature.arguments) {
      if (arg->is_matrix()) {
        formatter.format(", typename T{}", counter);
        ++counter;
      }
    }
  }
  formatter.format(">\n");

  if (signature.return_value) {
    if (!std::holds_alternative<ast::ScalarType>(*signature.return_value)) {
      // TODO: To support returning matrices in C++ we need more than just a `span` type.
      throw TypeError("Only scalars can be returned.");
    } else {
      formatter.format("auto {}(", signature.function_name);
    }
  } else {
    formatter.format("void {}(", signature.function_name);
  }

  std::size_t counter = 0;
  auto arg_printer = [&counter](CodeFormatter& formatter, const ast::Argument::shared_ptr& arg) {
    if (arg->is_matrix()) {
      if (arg->direction() == ast::ArgumentDirection::Input) {
        formatter.format("const T{}&", counter);
      } else {
        formatter.format("T{}&&", counter);
      }
      ++counter;
    } else {
      const NumericType numeric_type = std::get<ast::ScalarType>(arg->type()).numeric_type();
      if (arg->direction() == ast::ArgumentDirection::Input) {
        formatter.format("const {}", string_from_numeric_cast_type(numeric_type));
      } else {
        // Output reference for now.
        formatter.format("{}&", string_from_numeric_cast_type(numeric_type));
      }
    }

    formatter.format(" {}", arg->name());
  };

  formatter.join(std::move(arg_printer), ", ", signature.arguments);
  formatter.format(")\n");
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::ScalarType& scalar,
                                  const TypeContext context) const {
  const std::string_view numeric_type = string_from_numeric_cast_type(scalar.numeric_type());
  switch (context) {
    case TypeContext::InputArgument:
    case TypeContext::FunctionBody:
    case TypeContext::ReturnValue: {
      formatter.format(numeric_type);
      break;
    }
    case TypeContext::OutputArgument: {
      formatter.format("{}&", numeric_type);
      break;
    }
    case TypeContext::OptionalOutputArgument: {
      formatter.format("{}*", numeric_type);
      break;
    }
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Add& x) const {
  formatter.format("{} + {}", make_view(x.left), make_view(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter,
                                  const ast::AssignOutputArgument& assignment) const {
  const auto& dest_name = assignment.argument->name();
  const ast::Type& type = assignment.argument->type();

  if (std::holds_alternative<ast::MatrixType>(type)) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(type);
    auto range = make_range<std::size_t>(0, assignment.values.size());

    // TODO: If there is a unit dimension, use the [] operator?
    formatter.join(
        [&](CodeFormatter& fmt, std::size_t i) {
          const auto [row, col] = mat.compute_indices(i);
          fmt.format("{}{}({}, {}) = {};", assignment.argument->is_matrix() ? "_" : "", dest_name,
                     row, col, make_view(assignment.values[i]));
        },
        "\n", range);

  } else {
    // Otherwise it is a scalar, so just assign it:
    ZEN_ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("{} = {};", dest_name, assignment.values.front());
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

static constexpr std::string_view string_for_built_in_function_call(
    const BuiltInFunctionName name) {
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
    case BuiltInFunctionName::Abs:
      return "std::abs";
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
  if (x.function == BuiltInFunctionName::Signum) {
    // We need to special-case signum because it doesn't exist as a free-standing function in the
    // stl.
    // TODO: This should be an int expression.
    formatter.format(
        "static_cast<Scalar>(static_cast<Scalar>(0) < {}) - ({} < static_cast<Scalar>(0))",
        make_view(x.args[0]), make_view(x.args[0]));
  } else {
    formatter.format("{}({})", string_for_built_in_function_call(x.function),
                     make_join_view(*this, ", ", x.args));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  formatter.format("static_cast<{}>({})", string_from_numeric_cast_type(x.destination_type),
                   make_view(x.arg));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Compare& x) const {
  formatter.format("{} {} {}", make_view(x.left), string_from_relational_operation(x.operation),
                   make_view(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Branch& x) const {
  formatter.format("if ({}) ", make_view(x.condition));
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.if_branch); });
  if (!x.else_branch.empty()) {
    formatter.with_indentation(2, " else {\n", "\n}",
                               [&] { formatter.join(*this, "\n", x.else_branch); });
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter,
                                  const ast::ConstructReturnValue& x) const {
  ZEN_ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  ZEN_ASSERT_EQUAL(1, x.args.size());
  formatter.format("return {};", make_view(x.args[0]));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.format("{} {};", string_from_numeric_cast_type(x.type), x.name);
  } else {
    formatter.format("const {} {} = {};", string_from_numeric_cast_type(x.type), x.name,
                     make_view(x.value));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
  ZEN_ASSERT(x.argument);
  if (std::holds_alternative<ast::ScalarType>(x.argument->type())) {
    formatter.format(x.argument->name());
  } else {
    const ast::MatrixType& mat = std::get<ast::MatrixType>(x.argument->type());
    const auto [r, c] = mat.compute_indices(x.element);
    formatter.format("_{}({}, {})", x.argument->name(), r, c);
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.format("{} * {}", make_view(x.left), make_view(x.right));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter,
                                  const ast::OptionalOutputBranch& x) const {
  formatter.format("if (static_cast<bool>({}{})) ", x.argument->is_matrix() ? "_" : "",
                   x.argument->name());
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.statements); });
}

}  // namespace math
