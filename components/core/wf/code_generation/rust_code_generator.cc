// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/rust_code_generator.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"
#include "wf/utility/index_range.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {

constexpr std::string_view rust_string_from_numeric_type(const numeric_primitive_type type) {
  switch (type) {
    case numeric_primitive_type::boolean:
      return "bool";
    case numeric_primitive_type::integral:
      return "i64";
    case numeric_primitive_type::floating_point:
      return "f64";
  }
  throw type_error("Not a valid enum value: {}", string_from_code_numeric_type(type));
}

constexpr std::string_view span_type_from_direction(const argument_direction direction) {
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
  // We can fix these, but until we do let's disable them.
  result.push_back("clippy::unused_unit");
  result.push_back("clippy::collapsible_else_if");
  result.push_back("clippy::needless_late_init");
  result.push_back("unused_variables");
  result.push_back("unused_parens");
  return result;
}

std::string rust_code_generator::operator()(const matrix_type&) const {
  throw type_error(
      "The default Rust code-generator treats all matrices as spans. You likely want to implement "
      "a formatter override for the the `{}` type.",
      matrix_type::snake_case_name_str);
}

std::string rust_code_generator::operator()(const scalar_type& scalar) const {
  return std::string{rust_string_from_numeric_type(scalar.numeric_type())};
}

std::string rust_code_generator::operator()(const custom_type& custom) const {
  return custom.name();
}

std::string rust_code_generator::operator()(const argument& arg) const {
  std::string result = arg.name();
  result.append(": ");

  // Use generic type for spans:
  const std::string type = overloaded_visit(
      arg.type(), [&](matrix_type) { return fmt::format("T{}", arg.index()); },
      [&](const auto& others) { return operator()(others); });

  const bool pass_input_by_borrow = arg.is_matrix() || arg.is_custom_type();
  switch (arg.direction()) {
    case argument_direction::input:
      fmt::format_to(std::back_inserter(result), "{}{}", pass_input_by_borrow ? "&" : "", type);
      break;
    case argument_direction::output:
      fmt::format_to(std::back_inserter(result), "&mut {}", type);
      break;
    case argument_direction::optional_output:
      fmt::format_to(std::back_inserter(result), "Option<&mut {}>", type);
      break;
    default:
      WF_ASSERT_ALWAYS("Unhandled argument_direction: {}",
                       static_cast<std::size_t>(arg.direction()));
  }
  return result;
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

  // The argument list:
  result.append(">(");
  result += join(", ", signature.arguments(), *this);

  // Return value:
  if (const auto& maybe_return_type = signature.return_type(); maybe_return_type.has_value()) {
    std::visit(
        [&](const auto& type) {
          fmt::format_to(std::back_inserter(result), ") -> {}\n", make_view(type));
        },
        *maybe_return_type);
  } else {
    result.append(") -> ()\n");
  }

  // Constraints on matrix arguments:
  if (const std::vector<argument> matrix_args = signature.matrix_args(); !matrix_args.empty()) {
    join_and_indent(result, 2, "where\n", "\n", "\n", matrix_args, [](const argument& arg) {
      const matrix_type* mat = std::get_if<matrix_type>(&arg.type());
      WF_ASSERT(mat != nullptr);
      return fmt::format("T{}: wrenfold_traits::{}<{}, {}, ValueType = {}>,", arg.index(),
                         span_type_from_direction(arg.direction()), mat->rows(), mat->cols(),
                         rust_string_from_numeric_type(numeric_primitive_type::floating_point));
    });
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::add& x) const {
  return join(" + ", x.args, *this);
}

std::string rust_code_generator::operator()(const ast::assign_output_matrix& x) const {
  const auto range = make_range(static_cast<std::size_t>(0), x.value.type.size());
  return join("\n", range, [&](const std::size_t i) {
    const auto [row, col] = x.value.type.compute_indices(i);
    return fmt::format("{}.set({}, {}, {});", x.arg.name(), row, col, make_view(x.value.args[i]));
  });
}

std::string rust_code_generator::operator()(const ast::assign_output_scalar& x) const {
  return fmt::format("*{} = {};", x.arg.name(), make_view(x.value));
}

std::string rust_code_generator::operator()(const ast::assign_output_struct& x) const {
  return fmt::format("*{} = {};", x.arg.name(), make_view(x.value));
}

std::string rust_code_generator::operator()(const ast::assign_temporary& x) const {
  return fmt::format("{} = {};", x.left, make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::boolean_literal& x) const {
  return x.value ? "true" : "false";
}

std::string rust_code_generator::operator()(const ast::branch& x) const {
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if {} ", make_view(x.condition));
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.if_branch, *this);
  if (!x.else_branch.empty()) {
    join_and_indent(result, 2, " else {\n", "\n}", "\n", x.else_branch, *this);
  }
  return result;
}

static bool is_get_argument_with_custom_type(const ast::ast_element& var) {
  if (const auto get = ast::get_if<ast::get_argument>(var); get.has_value()) {
    return get->arg.is_custom_type();
  }
  return false;
}

std::string rust_code_generator::operator()(const ast::call_external_function& x) const {
  WF_ASSERT_EQ(x.args.size(), x.function.num_arguments());
  std::string result = x.function.name();
  result.append("(");
  result += join_enumerate(", ", x.args, [&](const std::size_t index, const ast::ast_element& v) {
    std::string arg_str = operator()(v);
    if (is_get_argument_with_custom_type(v)) {
      return arg_str;
    }
    if (const argument& arg = x.function.argument_at(index);
        arg.is_custom_type() || arg.is_matrix()) {
      // Borrow non-scalar arguments:
      arg_str.insert(0, "&");
    }
    return arg_str;
  });
  result.append(")");
  if (const scalar_type* s = std::get_if<scalar_type>(&x.function.return_type());
      static_cast<bool>(s) && s->numeric_type() == numeric_primitive_type::floating_point) {
    result.append(" as f64");
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::call_std_function& x) const {
  switch (x.function) {
    case std_math_function::cos:
    case std_math_function::sin:
    case std_math_function::tan:
    case std_math_function::acos:
    case std_math_function::asin:
    case std_math_function::atan:
    case std_math_function::sqrt:
    case std_math_function::cosh:
    case std_math_function::sinh:
    case std_math_function::tanh:
    case std_math_function::acosh:
    case std_math_function::asinh:
    case std_math_function::atanh:
    case std_math_function::abs:
      return fmt::format("({}).{}()", make_view(x[0]),
                         string_from_standard_library_function(x.function));
    case std_math_function::log:
      return fmt::format("({}).ln()", make_view(x[0]));
    case std_math_function::signum:
      return fmt::format("(0.0f64 < {arg}) as i64 - ({arg} < 0.0f64) as i64",
                         fmt::arg("arg", make_view(x[0])));
    case std_math_function::floor:
      return fmt::format("({}).floor() as i64", make_view(x[0]));
    case std_math_function::atan2:
    case std_math_function::powi:
    case std_math_function::powf:
      return fmt::format("({}).{}({})", make_view(x[0]),
                         string_from_standard_library_function(x.function), make_view(x[1]));
    default:
      break;
  }
  WF_ASSERT_ALWAYS("Unsupported standard library function: {}",
                   string_from_standard_library_function(x.function));
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
  // TODO: Parens are sometimes superfluous. They are only required if `left` and `right` are casts.
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
  std::string output = operator()(x.type);
  join_and_indent(output, 2, " {\n", "\n}", ",\n", x.field_values, [this](const auto& field_val) {
    const auto& [field_name, val] = field_val;
    return fmt::format("{}: {}", field_name, make_view(val));
  });
  return output;
}

std::string rust_code_generator::operator()(const ast::declaration& x) const {
  std::string output;
  std::visit(
      [&](const auto& type) {
        fmt::format_to(std::back_inserter(output), "let {}: {}", x.name, make_view(type));
      },
      x.type);
  if (x.value) {
    fmt::format_to(std::back_inserter(output), " = {};", make_view(*x.value));
  } else {
    output.append(";");
  }
  return output;
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
  if (ast::get_if<ast::get_argument>(x.arg)) {
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
  }
  return "<INVALID ENUM VALUE>";
}

std::string rust_code_generator::operator()(const ast::multiply& x) const {
  return join(" * ", x.args, *this);
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

std::string rust_code_generator::operator()(const ast::parenthetical& x) const {
  return fmt::format("({})", make_view(x.contents));
}

std::string rust_code_generator::operator()(const ast::special_constant& x) const {
  return std::string{rust_string_for_symbolic_constant(x.value)};
}

std::string rust_code_generator::operator()(const ast::ternary& x) const {
  return fmt::format("if {} {{ {} }} else {{ {} }}", make_view(x.condition), make_view(x.left),
                     make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

std::string rust_code_generator::operator()(const ast::ast_element& element) const {
  return visit(element, *this);
}

inline constexpr std::string_view preamble = R"code(//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

{code}
)code";

std::string rust_code_generator::apply_preamble(const std::string_view code) {
  WF_ASSERT(code.data());
  return fmt::format(preamble, fmt::arg("code", code));
}

}  // namespace wf
