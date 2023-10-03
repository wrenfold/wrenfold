#include "code_generation/rust_code_generator.h"

#include "code_generation/ast_formatters.h"

namespace math {

std::string RustCodeGenerator::generate_code(const ast::FunctionSignature& signature,
                                             const std::vector<ast::Variant>& body) const {
  CodeFormatter result{};
  format_signature(result, signature);
  result.with_indentation(2, "{\n", "\n}", [&] { result.join(*this, "\n", body); });
  return result.get_output();
}

constexpr std::string_view type_string_from_numeric_type(NumericType type) {
  switch (type) {
    case NumericType::Bool:
      return "bool";
    case NumericType::Integer:
      return "i64";
    case NumericType::Real:
      return "f64";
    case NumericType::Complex:
      throw TypeError("No complex number type yet in Rust");
  }
  throw TypeError("Not a valid enum value: {}", string_from_numeric_type(type));
}

constexpr std::string_view type_string_from_numeric_type(const ast::ScalarType& scalar) {
  return type_string_from_numeric_type(scalar.numeric_type());
}

constexpr std::string_view span_type_from_direction(ast::ArgumentDirection direction) {
  switch (direction) {
    case ast::ArgumentDirection::Input:
      return "Span2D";
    case ast::ArgumentDirection::Output:
    case ast::ArgumentDirection::OptionalOutput:
      return "OutputSpan2D";
  }
  return "<NOT A VALID ENUM VALUE>";
}

static std::vector<std::string_view> get_attributes(const ast::FunctionSignature& signature) {
  std::vector<std::string_view> result{};

  // TODO: Properly checking for snake case would require doing upper/lower case comparison with
  //  unicode support. Would need to add a dependency on ICU or some other external library to
  //  do this correctly. Maybe possible with wstring_convert, but that is deprecated and allegedly
  //  leaks memory on Windows.
  result.push_back("non_snake_case");

  // Check if # of args exceeds clippy warning.
  if (signature.arguments.size() >= 7) {
    result.push_back("clippy::too_many_arguments");
  }
  return result;
}

void RustCodeGenerator::format_signature(math::CodeFormatter& formatter,
                                         const ast::FunctionSignature& signature) const {
  formatter.format("#[inline]\n");

  const auto attributes = get_attributes(signature);
  if (!attributes.empty()) {
    formatter.format("#[allow({})]\n", fmt::join(attributes, ", "));
  }
  formatter.format("pub fn {}{}", signature.function_name,
                   signature.has_matrix_arguments() ? "<" : "");

  std::size_t counter = 0;
  for (const auto& arg : signature.arguments) {
    if (arg->is_matrix()) {
      formatter.format("T{}, ", counter++);
    }
  }

  formatter.format("{}(", signature.has_matrix_arguments() ? ">" : "");

  counter = 0;
  for (const auto& arg : signature.arguments) {
    formatter.format("{}: ", arg->name());

    std::string output_type;
    if (arg->is_matrix()) {
      output_type = fmt::format("T{}", counter);
      ++counter;
    } else {
      output_type = "f64";
    }

    switch (arg->direction()) {
      case ast::ArgumentDirection::Input:
        formatter.format("{}{}, ", arg->is_matrix() ? "&" : "", output_type);
        break;
      case ast::ArgumentDirection::Output:
        formatter.format("&mut {}, ", output_type);
        break;
      case ast::ArgumentDirection::OptionalOutput:
        formatter.format("Option<&mut {}>, ", output_type);
        break;
    }
  }

  if (!signature.return_value) {
    formatter.format(")\n");
  } else {
    const auto& scalar = std::get<ast::ScalarType>(*signature.return_value);
    formatter.format(") -> {}\n", type_string_from_numeric_type(scalar));
  }

  if (signature.has_matrix_arguments()) {
    formatter.with_indentation(2, "where\n", "", [&] {
      std::size_t counter = 0;
      for (const auto& arg : signature.arguments) {
        if (arg->is_matrix()) {
          const ast::MatrixType mat = std::get<ast::MatrixType>(arg->type());
          formatter.format("T{}: zen_traits::{}<{}, {}, ValueType = {}>,\n", counter++,
                           span_type_from_direction(arg->direction()), mat.rows(), mat.cols(),
                           type_string_from_numeric_type(NumericType::Real));
        }
      }
    });
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Add& x) const {
  formatter.format("{} + {}", make_view(x.left), make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::AssignOutputArgument& assignment) const {
  const auto& dest_name = assignment.argument->name();
  const ast::Type& type = assignment.argument->type();

  if (std::holds_alternative<ast::MatrixType>(type)) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(type);
    auto range = make_range<std::size_t>(0, assignment.values.size());
    formatter.join(
        [&](CodeFormatter& fmt, std::size_t i) {
          const auto [row, col] = mat.compute_indices(i);
          fmt.format("{}.set({}, {}, {});", dest_name, row, col, make_view(assignment.values[i]));
        },
        "\n", range);
  } else {
    // Otherwise it is a scalar, so just assign it:
    ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("*{} = {};", dest_name, assignment.values.front());
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Branch& x) const {
  ASSERT(x.condition);
  formatter.format("if {} ", make_view(x.condition));
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.if_branch); });

  if (!x.else_branch.empty()) {
    formatter.with_indentation(2, " else {\n", "\n}",
                               [&] { formatter.join(*this, "\n", x.else_branch); });
  }
}

static constexpr std::string_view string_for_built_in_function_call(
    const BuiltInFunctionName name) {
  switch (name) {
    case BuiltInFunctionName::Cos:
      return "f64::cos";
    case BuiltInFunctionName::Sin:
      return "f64::sin";
    case BuiltInFunctionName::Tan:
      return "f64::tan";
    case BuiltInFunctionName::ArcCos:
      return "f64::acos";
    case BuiltInFunctionName::ArcSin:
      return "f64::asin";
    case BuiltInFunctionName::ArcTan:
      return "f64::atan";
    case BuiltInFunctionName::Log:
      return "f64::ln";
    case BuiltInFunctionName::Sqrt:
      return "f64::sqrt";
    case BuiltInFunctionName::Abs:
      return "f64::abs";
    case BuiltInFunctionName::Signum:
      return "f64::signum";
    case BuiltInFunctionName::Arctan2:
      return "f64::atan2";
    case BuiltInFunctionName::Pow:
      return "f64::powf";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Call& x) const {
  formatter.format("{}({})", string_for_built_in_function_call(x.function),
                   make_join_view(*this, ", ", x.args));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  // TODO: Parens are sometimes superfluous here.
  formatter.format("({}) as {}", make_view(x.arg),
                   type_string_from_numeric_type(x.destination_type));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Compare& x) const {
  // TODO: Parens are sometimes superfluous.
  formatter.format("({}) {} ({})", make_view(x.left), string_from_relational_operation(x.operation),
                   make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::ConstructReturnValue& x) const {
  ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  ASSERT_EQUAL(1, x.args.size());
  formatter.format("{}", make_view(x.args[0]));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.format("let {}: {};", x.name, type_string_from_numeric_type(x.type));
  } else {
    formatter.format("let {}: {} = {};", x.name, type_string_from_numeric_type(x.type),
                     make_view(x.value));
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
  ASSERT(x.argument);
  if (x.argument->is_matrix()) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(x.argument->type());
    const auto [r, c] = mat.compute_indices(x.element);
    formatter.format("{}.get({}, {})", x.argument->name(), r, c);
  } else {
    formatter.format(x.argument->name());
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.format("{} * {}", make_view(x.left), make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::OptionalOutputBranch& x) const {
  formatter.format("if let Some({}) = {} ", x.argument->name(), x.argument->name());
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.statements); });
}

}  // namespace math
