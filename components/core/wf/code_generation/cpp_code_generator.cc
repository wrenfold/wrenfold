// Copyright 2023 Gareth Cross
#include "wf/code_generation/cpp_code_generator.h"

#include "wf/code_generation/ast_formatters.h"

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

constexpr static std::string_view cpp_string_from_numeric_cast_type(
    const NumericType destination_type) noexcept {
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
      throw type_error("Only scalars can be returned.");
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
        formatter.format("const {}", cpp_string_from_numeric_cast_type(numeric_type));
      } else {
        // Output reference for now.
        formatter.format("{}&", cpp_string_from_numeric_cast_type(numeric_type));
      }
    }

    formatter.format(" {}", arg->name());
  };

  formatter.join(std::move(arg_printer), ", ", signature.arguments);
  formatter.format(")\n");
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
    WF_ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("{} = {};", dest_name, assignment.values.front());
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

static constexpr std::string_view cpp_string_for_std_function(const StdMathFunction name) noexcept {
  switch (name) {
    case StdMathFunction::Cos:
      return "std::cos";
    case StdMathFunction::Sin:
      return "std::sin";
    case StdMathFunction::Tan:
      return "std::tan";
    case StdMathFunction::ArcCos:
      return "std::acos";
    case StdMathFunction::ArcSin:
      return "std::asin";
    case StdMathFunction::ArcTan:
      return "std::atan";
    case StdMathFunction::Log:
      return "std::log";
    case StdMathFunction::Sqrt:
      return "std::sqrt";
    case StdMathFunction::Abs:
      return "std::abs";
    case StdMathFunction::Arctan2:
      return "std::atan2";
    case StdMathFunction::Powi:
    case StdMathFunction::Powf:
      return "std::pow";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Call& x) const {
  if (x.function == StdMathFunction::Signum) {
    // We need to special-case signum because it doesn't exist as a free-standing function.
    // TODO: This should be an int expression.
    formatter.format(
        "static_cast<Scalar>(static_cast<Scalar>(0) < {}) - ({} < static_cast<Scalar>(0))",
        make_view(x.args[0]), make_view(x.args[0]));
  } else {
    formatter.format("{}({})", cpp_string_for_std_function(x.function),
                     make_join_view(*this, ", ", x.args));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  formatter.format("static_cast<{}>({})", cpp_string_from_numeric_cast_type(x.destination_type),
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
  WF_ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  WF_ASSERT_EQUAL(1, x.args.size());
  formatter.format("return {};", make_view(x.args[0]));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.format("{} {};", cpp_string_from_numeric_cast_type(x.type), x.name);
  } else {
    formatter.format("const {} {} = {};", cpp_string_from_numeric_cast_type(x.type), x.name,
                     make_view(x.value));
  }
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::Divide& x) const {
  formatter.format("{} / {}", make_view(x.left), make_view(x.right));
}

static constexpr std::string_view cpp_string_for_symbolic_constant(
    const SymbolicConstants value) noexcept {
  switch (value) {
    case SymbolicConstants::Euler:
      return "M_E";
    case SymbolicConstants::Pi:
      return "M_PI";
    case SymbolicConstants::True:
      return "true";
    case SymbolicConstants::False:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::SpecialConstant& x) const {
  formatter.format("static_cast<Scalar>({})", cpp_string_for_symbolic_constant(x.value));
}

void CppCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
  WF_ASSERT(x.argument);
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