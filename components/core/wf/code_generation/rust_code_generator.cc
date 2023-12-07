#include "wf/code_generation/rust_code_generator.h"

#include "wf/code_generation/ast_formatters.h"

namespace wf {

std::string rust_code_generator::generate_code(const function_signature& signature,
                                               const std::vector<ast::variant>& body) const {
  code_formatter result{};
  format_signature(result, signature);
  result.with_indentation(2, "{\n", "\n}", [&] { result.join(*this, "\n", body); });
  return result.get_output();
}

constexpr std::string_view type_string_from_numeric_type(code_numeric_type type) {
  switch (type) {
    case code_numeric_type::boolean:
      return "bool";
    case code_numeric_type::integral:
      return "i64";
    case code_numeric_type::floating_point:
      return "f64";
    case code_numeric_type::complex:
      throw type_error("No complex number type yet in Rust");
  }
  throw type_error("Not a valid enum value: {}", string_from_code_numeric_type(type));
}

constexpr std::string_view type_string_from_numeric_type(scalar_type scalar) {
  return type_string_from_numeric_type(scalar.numeric_type());
}

constexpr std::string_view span_type_from_direction(argument_direction direction) {
  switch (direction) {
    case argument_direction::input:
      return "Span2D";
    case argument_direction::output:
    case argument_direction::optional_output:
      return "OutputSpan2D";
  }
  return "<NOT A VALID ENUM VALUE>";
}

static std::vector<std::string_view> get_attributes(const function_signature& signature) {
  std::vector<std::string_view> result{};

  // TODO: Properly checking for snake case would require doing upper/lower case comparison with
  //  unicode support. Would need to add a dependency on ICU or some other external library to
  //  do this correctly. Maybe possible with wstring_convert, but that is deprecated and allegedly
  //  leaks memory on Windows.
  result.push_back("non_snake_case");

  // Check if # of args exceeds clippy warning.
  if (signature.num_arguments() >= 7) {
    result.push_back("clippy::too_many_arguments");
  }
  return result;
}

void rust_code_generator::format_signature(wf::code_formatter& formatter,
                                           const function_signature& signature) const {
  formatter.format("#[inline]\n");

  const auto attributes = get_attributes(signature);
  if (!attributes.empty()) {
    formatter.format("#[allow({})]\n", fmt::join(attributes, ", "));
  }
  formatter.format("pub fn {}{}", signature.name(), signature.has_matrix_arguments() ? "<" : "");

  std::size_t counter = 0;
  for (const auto& arg : signature.arguments()) {
    if (arg->is_matrix()) {
      formatter.format("T{}, ", counter++);
    }
  }

  formatter.format("{}(", signature.has_matrix_arguments() ? ">" : "");

  counter = 0;
  for (const auto& arg : signature.arguments()) {
    formatter.format("{}: ", arg->name());

    std::string output_type;
    if (arg->is_matrix()) {
      output_type = fmt::format("T{}", counter);
      ++counter;
    } else {
      output_type = "f64";
    }

    switch (arg->direction()) {
      case argument_direction::input:
        formatter.format("{}{}, ", arg->is_matrix() ? "&" : "", output_type);
        break;
      case argument_direction::output:
        formatter.format("&mut {}, ", output_type);
        break;
      case argument_direction::optional_output:
        formatter.format("Option<&mut {}>, ", output_type);
        break;
    }
  }

  if (!signature.has_return_value()) {
    formatter.format(")\n");
  } else {
    const auto return_type = *signature.return_value_type();
    if (const scalar_type* s = std::get_if<scalar_type>(&return_type); s != nullptr) {
      formatter.format(") -> {}\n", type_string_from_numeric_type(*s));
    } else {
      throw type_error("Return values must be scalars.");
    }
  }

  if (signature.has_matrix_arguments()) {
    formatter.with_indentation(2, "where\n", "", [&] {
      std::size_t counter = 0;
      for (const auto& arg : signature.arguments()) {
        if (arg->is_matrix()) {
          const matrix_type mat = std::get<matrix_type>(arg->type());
          formatter.format("T{}: wrenfold_traits::{}<{}, {}, ValueType = {}>,\n", counter++,
                           span_type_from_direction(arg->direction()), mat.rows(), mat.cols(),
                           type_string_from_numeric_type(code_numeric_type::floating_point));
        }
      }
    });
  }
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::add& x) const {
  formatter.format("{} + {}", make_view(x.left), make_view(x.right));
}

void rust_code_generator::operator()(code_formatter& formatter,
                                     const ast::assign_output_argument& assignment) const {
  const auto& dest_name = assignment.arg->name();
  const argument_type& type = assignment.arg->type();

  if (std::holds_alternative<matrix_type>(type)) {
    const matrix_type mat = std::get<matrix_type>(type);
    auto range = make_range<std::size_t>(0, assignment.values.size());
    formatter.join(
        [&](code_formatter& fmt, std::size_t i) {
          const auto [row, col] = mat.compute_indices(i);
          fmt.format("{}.set({}, {}, {});", dest_name, row, col, make_view(assignment.values[i]));
        },
        "\n", range);
  } else {
    // Otherwise it is a scalar, so just assign it:
    WF_ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("*{} = {};", dest_name, make_view(assignment.values.front()));
  }
}

void rust_code_generator::operator()(code_formatter& formatter,
                                     const ast::assign_temporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::branch& x) const {
  WF_ASSERT(x.condition);
  formatter.format("if {} ", make_view(x.condition));
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.if_branch); });

  if (!x.else_branch.empty()) {
    formatter.with_indentation(2, " else {\n", "\n}",
                               [&] { formatter.join(*this, "\n", x.else_branch); });
  }
}

static constexpr std::string_view rust_string_for_std_function(
    const std_math_function name) noexcept {
  switch (name) {
    case std_math_function::cos:
      return "f64::cos";
    case std_math_function::sin:
      return "f64::sin";
    case std_math_function::tan:
      return "f64::tan";
    case std_math_function::acos:
      return "f64::acos";
    case std_math_function::asin:
      return "f64::asin";
    case std_math_function::atan:
      return "f64::atan";
    case std_math_function::log:
      return "f64::ln";
    case std_math_function::sqrt:
      return "f64::sqrt";
    case std_math_function::abs:
      return "f64::abs";
    case std_math_function::signum:
      return "f64::signum";
    case std_math_function::atan2:
      return "f64::atan2";
    case std_math_function::powi:
      return "f64::powi";
    case std_math_function::powf:
      return "f64::powf";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::call& x) const {
  // We have to override signum specially here, because the built-in rust signum does not return 0.
  if (x.function == std_math_function::signum) {
    // TODO: should be an integer expression:
    formatter.format("((0.0f64 < {}) as i64 - ({} < 0.0f64) as i64) as f64", make_view(x.args[0]),
                     make_view(x.args[0]));
  } else {
    formatter.format("{}({})", rust_string_for_std_function(x.function),
                     make_join_view(*this, ", ", x.args));
  }
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::cast& x) const {
  // TODO: Parens are sometimes superfluous here.
  formatter.format("({}) as {}", make_view(x.arg),
                   type_string_from_numeric_type(x.destination_type));
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::compare& x) const {
  // TODO: Parens are sometimes superfluous.
  formatter.format("({}) {} ({})", make_view(x.left), string_from_relational_operation(x.operation),
                   make_view(x.right));
}

void rust_code_generator::operator()(code_formatter& formatter,
                                     const ast::construct_return_value& x) const {
  WF_ASSERT(std::holds_alternative<scalar_type>(x.type), "We cannot return matrices");
  WF_ASSERT_EQUAL(1, x.args.size());
  formatter.format("{}", make_view(x.args[0]));
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::declaration& x) const {
  if (!x.value) {
    formatter.format("let {}: {};", x.name, type_string_from_numeric_type(x.type));
  } else {
    formatter.format("let {}: {} = {};", x.name, type_string_from_numeric_type(x.type),
                     make_view(x.value));
  }
}

void rust_code_generator::operator()(wf::code_formatter& formatter, const ast::divide& x) const {
  formatter.format("{} / {}", make_view(x.left), make_view(x.right));
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::input_value& x) const {
  WF_ASSERT(x.arg);
  if (x.arg->is_matrix()) {
    const matrix_type mat = std::get<matrix_type>(x.arg->type());
    const auto [r, c] = mat.compute_indices(x.element);
    formatter.format("{}.get({}, {})", x.arg->name(), r, c);
  } else {
    formatter.format(x.arg->name());
  }
}

static constexpr std::string_view rust_string_for_symbolic_constant(
    const symbolic_constant_enum value) noexcept {
  switch (value) {
    case symbolic_constant_enum::euler:
      return "std::f64::consts::E";
    case symbolic_constant_enum::pi:
      return "std::f64::consts::PI";
    case symbolic_constant_enum::boolean_true:
      return "true";
    case symbolic_constant_enum::boolean_false:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

void rust_code_generator::operator()(code_formatter& formatter,
                                     const ast::special_constant& x) const {
  formatter.format("{}", rust_string_for_symbolic_constant(x.value));
}

void rust_code_generator::operator()(code_formatter& formatter, const ast::multiply& x) const {
  formatter.format("{} * {}", make_view(x.left), make_view(x.right));
}

void rust_code_generator::operator()(wf::code_formatter& formatter, const ast::negate& x) const {
  formatter.format("-{}", make_view(x.arg));
}

void rust_code_generator::operator()(code_formatter& formatter,
                                     const ast::optional_output_branch& x) const {
  formatter.format("if let Some({}) = {} ", x.arg->name(), x.arg->name());
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.statements); });
}

}  // namespace wf
