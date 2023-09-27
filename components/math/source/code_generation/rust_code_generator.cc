#include "code_generation/rust_code_generator.h"

#include "code_generation/ast_formatters.h"

namespace math {

std::string RustCodeGenerator::Generate(const ast::FunctionSignature& signature,
                                        const std::vector<ast::Variant>& body) const {
  CodeFormatter result{};
  FormatSignature(result, signature);
  result.WithIndentation(2, "{\n", "\n}", [&] { result.Join(*this, "\n", body); });
  return result.GetOutput();
}

constexpr std::string_view TypeFromNumericType(NumericType type) {
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
  throw TypeError("Not a valid enum value: {}", StringFromNumericType(type));
}

constexpr std::string_view TypeFromNumericType(const ast::ScalarType& scalar) {
  return TypeFromNumericType(scalar.GetNumericType());
}

constexpr std::string_view SpanTypeFromDirection(ast::ArgumentDirection direction) {
  switch (direction) {
    case ast::ArgumentDirection::Input:
      return "Span2D";
    case ast::ArgumentDirection::Output:
    case ast::ArgumentDirection::OptionalOutput:
      return "OutputSpan2D";
  }
  return "<NOT A VALID ENUM VALUE>";
}

static std::vector<std::string_view> GetAttributes(const ast::FunctionSignature& signature) {
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

void RustCodeGenerator::FormatSignature(math::CodeFormatter& formatter,
                                        const ast::FunctionSignature& signature) const {
  formatter.Format("#[inline]\n");

  const auto attributes = GetAttributes(signature);
  if (!attributes.empty()) {
    formatter.Format("#[allow({})]\n", fmt::join(attributes, ", "));
  }
  formatter.Format("pub fn {}{}", signature.function_name,
                   signature.HasMatrixArguments() ? "<" : "");

  std::size_t counter = 0;
  for (const auto& arg : signature.arguments) {
    if (arg->IsMatrix()) {
      formatter.Format("T{}, ", counter++);
    }
  }

  formatter.Format("{}(", signature.HasMatrixArguments() ? ">" : "");

  counter = 0;
  for (const auto& arg : signature.arguments) {
    formatter.Format("{}: ", arg->Name());

    std::string output_type;
    if (arg->IsMatrix()) {
      output_type = fmt::format("T{}", counter);
      ++counter;
    } else {
      output_type = "f64";
    }

    switch (arg->Direction()) {
      case ast::ArgumentDirection::Input:
        formatter.Format("{}{}, ", arg->IsMatrix() ? "&" : "", output_type);
        break;
      case ast::ArgumentDirection::Output:
        formatter.Format("&mut {}, ", output_type);
        break;
      case ast::ArgumentDirection::OptionalOutput:
        formatter.Format("Option<&mut {}>, ", output_type);
        break;
    }
  }

  if (!signature.return_value) {
    formatter.Format(")\n");
  } else {
    const auto& scalar = std::get<ast::ScalarType>(*signature.return_value);
    formatter.Format(") -> {}\n", TypeFromNumericType(scalar));
  }

  if (signature.HasMatrixArguments()) {
    formatter.WithIndentation(2, "where\n", "", [&] {
      std::size_t counter = 0;
      for (const auto& arg : signature.arguments) {
        if (arg->IsMatrix()) {
          const ast::MatrixType mat = std::get<ast::MatrixType>(arg->Type());
          formatter.Format("T{}: zen_traits::{}<{}, {}, ValueType = {}>,\n", counter++,
                           SpanTypeFromDirection(arg->Direction()), mat.NumRows(), mat.NumCols(),
                           TypeFromNumericType(NumericType::Real));
        }
      }
    });
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Add& x) const {
  formatter.Format("{} + {}", View(x.left), View(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::AssignOutputArgument& assignment) const {
  const auto& dest_name = assignment.argument->Name();
  const ast::Type& type = assignment.argument->Type();

  if (std::holds_alternative<ast::MatrixType>(type)) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(type);
    auto range = MakeRange<std::size_t>(0, assignment.values.size());
    formatter.Join(
        [&](CodeFormatter& fmt, std::size_t i) {
          const auto [row, col] = mat.ComputeIndices(i);
          fmt.Format("{}.set({}, {}, {});", dest_name, row, col, View(assignment.values[i]));
        },
        "\n", range);
  } else {
    // Otherwise it is a scalar, so just assign it:
    ASSERT_EQUAL(1, assignment.values.size());
    formatter.Format("*{} = {};", dest_name, assignment.values.front());
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.Format("{} = {};", x.left, View(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Branch& x) const {
  ASSERT(x.condition);
  formatter.Format("if {} ", View(x.condition));
  formatter.WithIndentation(2, "{\n", "\n}", [&] { formatter.Join(*this, "\n", x.if_branch); });

  if (!x.else_branch.empty()) {
    formatter.WithIndentation(2, " else {\n", "\n}",
                              [&] { formatter.Join(*this, "\n", x.else_branch); });
  }
}

static constexpr std::string_view GetBuiltInFunctionCall(const BuiltInFunctionName name) {
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
  formatter.Format("{}({})", GetBuiltInFunctionCall(x.function), Join(*this, ", ", x.args));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Cast& x) const {
  // TODO: Parens are sometimes superfluous here.
  formatter.Format("({}) as {}", View(x.arg), TypeFromNumericType(x.destination_type));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Compare& x) const {
  // TODO: Parens are sometimes superfluous.
  formatter.Format("({}) {} ({})", View(x.left), StringFromRelationalOperation(x.operation),
                   View(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::ConstructReturnValue& x) const {
  ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  ASSERT_EQUAL(1, x.args.size());
  formatter.Format("{}", View(x.args[0]));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Declaration& x) const {
  if (!x.value) {
    formatter.Format("let {}: {};", x.name, TypeFromNumericType(x.type));
  } else {
    formatter.Format("let {}: {} = {};", x.name, TypeFromNumericType(x.type), View(x.value));
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
  ASSERT(x.argument);
  if (x.argument->IsMatrix()) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(x.argument->Type());
    const auto [r, c] = mat.ComputeIndices(x.element);
    formatter.Format("{}.get({}, {})", x.argument->Name(), r, c);
  } else {
    formatter.Format(x.argument->Name());
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Multiply& x) const {
  formatter.Format("{} * {}", View(x.left), View(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter,
                                   const ast::OptionalOutputBranch& x) const {
  formatter.Format("if let Some({}) = {} ", x.argument->Name(), x.argument->Name());
  formatter.WithIndentation(2, "{\n", "\n}", [&] { formatter.Join(*this, "\n", x.statements); });
}

}  // namespace math
