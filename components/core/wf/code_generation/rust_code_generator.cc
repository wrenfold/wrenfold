#include "wf/code_generation/rust_code_generator.h"

#include "wf/code_generation/ast_formatters.h"

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
      throw type_error("No complex number type yet in Rust");
  }
  throw type_error("Not a valid enum value: {}", string_from_numeric_type(type));
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
          formatter.format("T{}: wrenfold_traits::{}<{}, {}, ValueType = {}>,\n", counter++,
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
    WF_ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("*{} = {};", dest_name, assignment.values.front());
  }
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::AssignTemporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Branch& x) const {
  WF_ASSERT(x.condition);
  formatter.format("if {} ", make_view(x.condition));
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.if_branch); });

  if (!x.else_branch.empty()) {
    formatter.with_indentation(2, " else {\n", "\n}",
                               [&] { formatter.join(*this, "\n", x.else_branch); });
  }
}

static constexpr std::string_view rust_string_for_std_function(
    const StdMathFunction name) noexcept {
  switch (name) {
    case StdMathFunction::Cos:
      return "f64::cos";
    case StdMathFunction::Sin:
      return "f64::sin";
    case StdMathFunction::Tan:
      return "f64::tan";
    case StdMathFunction::ArcCos:
      return "f64::acos";
    case StdMathFunction::ArcSin:
      return "f64::asin";
    case StdMathFunction::ArcTan:
      return "f64::atan";
    case StdMathFunction::Log:
      return "f64::ln";
    case StdMathFunction::Sqrt:
      return "f64::sqrt";
    case StdMathFunction::Abs:
      return "f64::abs";
    case StdMathFunction::Signum:
      return "f64::signum";
    case StdMathFunction::Arctan2:
      return "f64::atan2";
    case StdMathFunction::Powi:
      return "f64::powi";
    case StdMathFunction::Powf:
      return "f64::powf";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::Call& x) const {
  // We have to override signum specially here, because the built-in rust signum does not return 0.
  if (x.function == StdMathFunction::Signum) {
    // TODO: should be an integer expression:
    formatter.format("((0.0f64 < {}) as i64 - ({} < 0.0f64) as i64) as f64", make_view(x.args[0]),
                     make_view(x.args[0]));
  } else {
    formatter.format("{}({})", rust_string_for_std_function(x.function),
                     make_join_view(*this, ", ", x.args));
  }
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
  WF_ASSERT(std::holds_alternative<ast::ScalarType>(x.type), "We cannot return matrices");
  WF_ASSERT_EQUAL(1, x.args.size());
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

void RustCodeGenerator::operator()(math::CodeFormatter& formatter, const ast::Divide& x) const {
  formatter.format("{} / {}", make_view(x.left), make_view(x.right));
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::InputValue& x) const {
  WF_ASSERT(x.argument);
  if (x.argument->is_matrix()) {
    const ast::MatrixType mat = std::get<ast::MatrixType>(x.argument->type());
    const auto [r, c] = mat.compute_indices(x.element);
    formatter.format("{}.get({}, {})", x.argument->name(), r, c);
  } else {
    formatter.format(x.argument->name());
  }
}

static constexpr std::string_view rust_string_for_symbolic_constant(
    const SymbolicConstants value) noexcept {
  switch (value) {
    case SymbolicConstants::Euler:
      return "std::f64::consts::E";
    case SymbolicConstants::Pi:
      return "std::f64::consts::PI";
    case SymbolicConstants::True:
      return "true";
    case SymbolicConstants::False:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

void RustCodeGenerator::operator()(CodeFormatter& formatter, const ast::SpecialConstant& x) const {
  formatter.format("{}", rust_string_for_symbolic_constant(x.value));
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