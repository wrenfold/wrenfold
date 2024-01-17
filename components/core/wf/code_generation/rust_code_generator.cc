#include "wf/code_generation/rust_code_generator.h"

#include "fmt/chrono.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/index_range.h"
#include "wf/template_utils.h"

namespace wf {

constexpr std::string_view rust_string_from_numeric_type(code_numeric_type type) {
  switch (type) {
    case code_numeric_type::boolean:
      return "bool";
    case code_numeric_type::integral:
      return "i64";
    case code_numeric_type::floating_point:
      return "f64";
  }
  throw type_error("Not a valid enum value: {}", string_from_code_numeric_type(type));
}

constexpr std::string_view rust_string_from_numeric_type(scalar_type scalar) {
  return rust_string_from_numeric_type(scalar.numeric_type());
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

static std::vector<std::string_view> get_attributes(const ast::function_signature& signature) {
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
  // We can fix these three, but until we do let's disable them.
  result.push_back("clippy::unused_unit");
  result.push_back("clippy::collapsible_else_if");
  result.push_back("unused_variables");
  return result;
}

std::string rust_code_generator::operator()(const argument& arg) const {
  std::string result;
  fmt::format_to(std::back_inserter(result), "{}: ", arg.name());

  // Use generic type for spans:
  const std::string output_type = overloaded_visit(
      arg.type(),
      [](const scalar_type s) -> std::string {
        return std::string(rust_string_from_numeric_type(s));
      },
      [&](matrix_type) { return fmt::format("T{}", arg.index()); },
      [this](const custom_type& c) { return operator()(c); });

  const bool pass_input_by_borrow = arg.is_matrix() || arg.is_custom_type();
  switch (arg.direction()) {
    case argument_direction::input:
      fmt::format_to(std::back_inserter(result), "{}{}", pass_input_by_borrow ? "&" : "",
                     output_type);
      break;
    case argument_direction::output:
      fmt::format_to(std::back_inserter(result), "&mut {}", output_type);
      break;
    case argument_direction::optional_output:
      fmt::format_to(std::back_inserter(result), "Option<&mut {}>", output_type);
      break;
  }
  return result;
}

std::string rust_code_generator::operator()(const custom_type& custom) const {
  return custom.name();
}

std::string rust_code_generator::operator()(const ast::function_definition& definition) const {
  std::string result = operator()(definition.signature());
  join_and_indent(result, 2, "{\n", "\n}", "\n", definition.body(), *this);
  return result;
}

std::string rust_code_generator::operator()(const ast::function_signature& signature) const {
  std::string result = "#[inline]\n";
  if (const auto attributes = get_attributes(signature); !attributes.empty()) {
    fmt::format_to(std::back_inserter(result), "#[allow({})]\n", fmt::join(attributes, ", "));
  }

  // Print the function name, then the list of generic parameters.
  fmt::format_to(std::back_inserter(result), "pub fn {}<", signature.name());
  for (const argument& arg : signature.arguments()) {
    if (arg.is_matrix()) {
      fmt::format_to(std::back_inserter(result), "T{}, ", arg.index());
    }
  }
  result.append(">(");
  join_to(result, ", ", signature.arguments(), *this);

  // Return value:
  fmt::format_to(std::back_inserter(result), ") -> {}\n", make_view(signature.return_annotation()));

  // Constraints on matrix arguments:
  if (const std::vector<argument> matrix_args = signature.matrix_args(); !matrix_args.empty()) {
    join_and_indent(result, 2, "where\n", "\n", "\n", matrix_args, [](const argument& arg) {
      const matrix_type* mat = std::get_if<matrix_type>(&arg.type());
      WF_ASSERT(mat != nullptr);
      return fmt::format("T{}: wrenfold_traits::{}<{}, {}, ValueType = {}>,", arg.index(),
                         span_type_from_direction(arg.direction()), mat->rows(), mat->cols(),
                         rust_string_from_numeric_type(code_numeric_type::floating_point));
    });
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::return_type_annotation& x) const {
  if (!x.type) {
    return "()";
  }
  return overloaded_visit(
      x.type.value(),
      [&](const scalar_type scalar) -> std::string {
        return std::string{rust_string_from_numeric_type(scalar)};
      },
      [&](matrix_type) -> std::string {
        throw type_error(
            "The default Rust code-generator treats all matrices as spans. We cannot return one "
            "directly. You likely want to implement an override for the {} ast type.",
            ast::return_type_annotation::snake_case_name_str);
      },
      [&](const custom_type& custom_type) { return operator()(custom_type); });
}

std::string rust_code_generator::operator()(const ast::add& x) const {
  return fmt::format("{} + {}", make_view(x.left), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::assign_output_matrix& x) const {
  const auto range = make_range(static_cast<std::size_t>(0), x.value->type.size());
  return join(
      [&](const std::size_t i) {
        const auto [row, col] = x.value->type.compute_indices(i);
        return fmt::format("{}.set({}, {}, {});", x.arg.name(), row, col,
                           make_view(x.value->args[i]));
      },
      "\n", range);
}

std::string rust_code_generator::operator()(const ast::assign_output_scalar& x) const {
  return fmt::format("*{} = {};", x.arg.name(), make_view(x.value));
}

std::string rust_code_generator::operator()(const ast::assign_output_struct& x) const {
  if (x.arg.is_optional()) {
    return fmt::format("*{} = {};", x.arg.name(), make_view(*x.value));
  } else {
    return fmt::format("{} = {};", x.arg.name(), make_view(*x.value));
  }
}

std::string rust_code_generator::operator()(const ast::assign_temporary& x) const {
  return fmt::format("{} = {};", x.left, make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::branch& x) const {
  WF_ASSERT(x.condition);
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if {} ", make_view(x.condition));
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.if_branch, *this);
  if (!x.else_branch.empty()) {
    join_and_indent(result, 2, " else {\n", "\n}", "\n", x.else_branch, *this);
  }
  return result;
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

std::string rust_code_generator::operator()(const ast::call_custom_function& x) const {
  WF_ASSERT_EQUAL(x.args.size(), x.function.num_arguments());
  std::size_t index = 0;
  std::string result = x.function.name();
  result.append("(");
  join_to(result, ", ", x.args, [&](const ast::variant& v) {
    std::string arg_str = operator()(v);
    if (const argument& arg = x.function.arguments()[index++];
        arg.is_custom_type() || arg.is_matrix()) {
      // Borrow non-scalar arguments:
      arg_str.insert(0, "&");
    }
    return arg_str;
  });
  result.append(")");
  if (const scalar_type* s = std::get_if<scalar_type>(&x.function.return_type());
      static_cast<bool>(s) && s->numeric_type() == code_numeric_type::floating_point) {
    result.append(" as f64");
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::call_std_function& x) const {
  // We have to override signum specially here, because the built-in rust signum does not return 0.
  if (x.function == std_math_function::signum) {
    // TODO: should be an integer expression:
    return fmt::format("((0.0f64 < {}) as i64 - ({} < 0.0f64) as i64) as f64", make_view(x.args[0]),
                       make_view(x.args[0]));
  } else {
    const std::string args = join(*this, ", ", x.args);
    return fmt::format("{}({})", rust_string_for_std_function(x.function), args);
  }
}

std::string rust_code_generator::operator()(const ast::cast& x) const {
  // TODO: Parens are sometimes superfluous here.
  return fmt::format("({}) as {}", make_view(x.arg),
                     rust_string_from_numeric_type(x.destination_type));
}

std::string rust_code_generator::operator()(const ast::comment& x) const {
  const std::vector<std::string> lines = x.split_lines();
  std::string result{};
  for (const auto& line : lines) {
    fmt::format_to(std::back_inserter(result), "// {}\n", line);
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::compare& x) const {
  // TODO: Parens are sometimes superfluous.
  return fmt::format("({}) {} ({})", make_view(x.left),
                     string_from_relational_operation(x.operation), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::construct_matrix&) const {
  throw type_error(
      "The default Rust code-generator treats all matrices as span traits. We cannot construct "
      "one directly. You likely want to implement an override for the the ConstructMatrix ast "
      "type.");
}

std::string rust_code_generator::operator()(const ast::construct_custom_type& x) const {
  const std::string opener = fmt::format("{} {{\n", make_view(x.type));
  std::string output{};
  join_and_indent(output, 2, opener, "\n}", ",\n", x.field_values, [this](const auto& field_val) {
    const auto& [field_name, val] = field_val;
    return fmt::format("{}: {}", field_name, make_view(val));
  });
  return output;
}

std::string rust_code_generator::operator()(const ast::declaration& x) const {
  std::string output;
  fmt::format_to(std::back_inserter(output), "let {}: {}", x.name, operator()(x.type));
  if (x.value) {
    fmt::format_to(std::back_inserter(output), " = {};", make_view(*x.value));
  } else {
    output.append(";");
  }
  return output;
}

std::string rust_code_generator::operator()(const ast::declaration_type_annotation& x) const {
  return overloaded_visit(
      x.type,
      [](const scalar_type s) -> std::string {
        return std::string(rust_string_from_numeric_type(s));
      },
      [](const matrix_type&) -> std::string {
        throw type_error(
            "The default Rust code-generator treats all matrices as span traits. We cannot "
            "construct one directly. You likely want to implement an override for the the `{}` ast "
            "type.",
            ast::declaration_type_annotation::snake_case_name_str);
      },
      [](const custom_type& c) -> std::string { return c.name(); });
}

std::string rust_code_generator::operator()(const ast::divide& x) const {
  return fmt::format("{} / {}", make_view(x.left), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::float_literal& x) const {
  return fmt::format("{}f64", x.value);
}

std::string rust_code_generator::operator()(const ast::get_argument& x) const {
  return x.arg.name();
}

std::string rust_code_generator::operator()(const ast::get_field& x) const {
  return fmt::format("{}.{}", make_view(x.arg), x.field);
}

std::string rust_code_generator::operator()(const ast::get_matrix_element& x) const {
  if (std::holds_alternative<ast::get_argument>(*x.arg)) {
    // This will be a span trait.
    return fmt::format("{}.get({}, {})", make_view(x.arg), x.row, x.col);
  } else {
    // Otherwise, take a guess at what the matrix type will prefer.
    return fmt::format("{}[({}, {})]", make_view(x.arg), x.row, x.col);
  }
}

std::string rust_code_generator::operator()(const ast::integer_literal& x) const {
  return fmt::format("{}i64", x.value);
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

std::string rust_code_generator::operator()(const ast::multiply& x) const {
  return fmt::format("{} * {}", make_view(x.left), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::negate& x) const {
  return fmt::format("-{}", make_view(x.arg));
}

std::string rust_code_generator::operator()(const ast::optional_output_branch& x) const {
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if let Some({}) = {} ", x.arg.name(), x.arg.name());
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.statements, *this);
  return result;
}

std::string rust_code_generator::operator()(const ast::return_object& x) const {
  return operator()(x.value);
}

std::string rust_code_generator::operator()(const ast::special_constant& x) const {
  return std::string{rust_string_for_symbolic_constant(x.value)};
}

std::string rust_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

}  // namespace wf
