#include "wf/code_generation/rust_code_generator.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/index_range.h"

namespace wf {

std::string rust_code_generator::generate_code(const function_signature& signature,
                                               const std::vector<ast::variant>& body) const {
  std::string result = format_signature(signature);
  join_and_indent(result, 2, "{\n", "\n}", "\n", body, *this);
  return result;
}

constexpr std::string_view type_string_from_numeric_type(code_numeric_type type) {
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

// Assign indices to arguments that need generics declared. (Just matrices for now).
// Returns new vector with _only_ the args that need generics.
static auto assign_labels_to_generic_args(const std::vector<argument::shared_ptr>& args) {
  std::vector<std::tuple<std::string, argument::shared_ptr>> result;
  result.reserve(args.size());
  std::size_t counter = 0;
  for (const auto& arg : args) {
    if (arg->is_matrix()) {
      result.emplace_back(fmt::format("T{}", counter), arg);
      ++counter;
    }
  }
  return result;
}

std::string rust_code_generator::format_signature(const function_signature& signature) const {
  std::string result = "#[inline]\n";
  if (const auto attributes = get_attributes(signature); !attributes.empty()) {
    fmt::format_to(std::back_inserter(result), "#[allow({})]\n", fmt::join(attributes, ", "));
  }

  // Assign generic naames to arguments that require them:
  const auto generic_args = assign_labels_to_generic_args(signature.arguments());

  // Print the function name, then the list of generic parameters.
  fmt::format_to(std::back_inserter(result), "pub fn {}", signature.name());
  if (!generic_args.empty()) {
    result.append("<");
    for (const auto& [generic, _] : generic_args) {
      fmt::format_to(std::back_inserter(result), "{}, ", generic);
    }
    result.append(">");
  }

  result.append("(");
  for (const auto& arg : signature.arguments()) {
    fmt::format_to(std::back_inserter(result), "{}: ", arg->name());

    // Check if this arg has a generic label:
    const auto generic_it =
        std::find_if(generic_args.begin(), generic_args.end(),
                     [&](const auto& pair) { return std::get<1>(pair) == arg; });
    const std::string output_type =
        generic_it != generic_args.end() ? std::get<0>(*generic_it) : "f64";

    switch (arg->direction()) {
      case argument_direction::input:
        fmt::format_to(std::back_inserter(result), "{}{}, ", arg->is_matrix() ? "&" : "",
                       output_type);
        break;
      case argument_direction::output:
        fmt::format_to(std::back_inserter(result), "&mut {}, ", output_type);
        break;
      case argument_direction::optional_output:
        fmt::format_to(std::back_inserter(result), "Option<&mut {}>, ", output_type);
        break;
    }
  }

  if (!signature.has_return_value()) {
    result.append(")\n");
  } else {
    const auto return_type = *signature.return_value_type();
    overloaded_visit(
        return_type,
        [&](scalar_type scalar) {
          fmt::format_to(std::back_inserter(result), ") -> {}\n",
                         type_string_from_numeric_type(scalar));
        },
        [&](matrix_type) {
          throw type_error(
              "Matrices cannot be directly returned in C++ (since they are passed as spans).");
        },
        [&](const custom_type::const_shared_ptr& custom_type) {
          // TODO: This needs to be overridable from python.
          fmt::format_to(std::back_inserter(result), ") -> {}\n", custom_type->name());
        });
  }

  if (!generic_args.empty()) {
    join_and_indent(result, 2, "where\n", "\n", "\n", generic_args, [](const auto& label_and_arg) {
      const auto& [label, arg] = label_and_arg;
      const matrix_type* mat = std::get_if<matrix_type>(&arg->type());
      WF_ASSERT(mat != nullptr);
      return fmt::format("{}: wrenfold_traits::{}<{}, {}, ValueType = {}>,", label,
                         span_type_from_direction(arg->direction()), mat->rows(), mat->cols(),
                         type_string_from_numeric_type(code_numeric_type::floating_point));
    });
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::add& x) const {
  return fmt::format("{} + {}", make_view(x.left), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::assign_output_argument& assignment) const {
  const auto& dest_name = assignment.arg->name();
  const type_variant& type = assignment.arg->type();

  return overloaded_visit(
      type,
      [&](matrix_type mat) {
        const auto range = make_range<std::size_t>(0, assignment.values.size());
        return join(
            [&](std::size_t i) {
              const auto [row, col] = mat.compute_indices(i);
              return fmt::format("{}.set({}, {}, {});", dest_name, row, col,
                                 make_view(assignment.values[i]));
            },
            "\n", range);
      },
      [&](scalar_type) {
        WF_ASSERT_EQUAL(1, assignment.values.size());
        return fmt::format("*{} = {};", dest_name, make_view(assignment.values.front()));
      },
      [&](const custom_type::const_shared_ptr&) -> std::string {
        throw type_error("TODO: Implement me");
      });
}

std::string rust_code_generator::operator()(const ast::assign_temporary& x) const {
  return fmt::format("{} = {};", x.left, make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::branch& x) const {
  WF_ASSERT(x.condition);
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if {}", make_view(x.condition));
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

std::string rust_code_generator::operator()(const ast::call& x) const {
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
                     type_string_from_numeric_type(x.destination_type));
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
      "The default Rust code-generator treats all matrices as spant traits. We cannot construct "
      "one directly. You likely want to implement an override for the the ConstructMatrix ast "
      "type.");
}

std::string rust_code_generator::operator()(const ast::construct_custom_type& x) const {
  const std::string opener = fmt::format("{} {{\n", x.type->name());
  std::string output{};
  join_and_indent(output, 2, opener, "\n}", ",\n", x.field_values, [this](const auto& field_val) {
    const auto& [field_name, val] = field_val;
    return fmt::format("{}: {}", field_name, make_view(val));
  });
  return output;
}

std::string rust_code_generator::operator()(const ast::declaration& x) const {
  if (!x.value) {
    return fmt::format("let {}: {};", x.name, type_string_from_numeric_type(x.type));
  } else {
    return fmt::format("let {}: {} = {};", x.name, type_string_from_numeric_type(x.type),
                       make_view(x.value));
  }
}

std::string rust_code_generator::operator()(const ast::divide& x) const {
  return fmt::format("{} / {}", make_view(x.left), make_view(x.right));
}

std::string rust_code_generator::operator()(const ast::float_literal& x) const {
  return fmt::format("{}f64", x.value);
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
  fmt::format_to(std::back_inserter(result), "if let Some({}) = {} ", x.arg->name(), x.arg->name());
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.statements, *this);
  return result;
}

std::string rust_code_generator::operator()(const ast::read_input_matrix& x) const {
  WF_ASSERT(x.arg);
  return fmt::format("{}.get({}, {})", x.arg->name(), x.row, x.col);
}

std::string rust_code_generator::operator()(const ast::read_input_scalar& x) const {
  WF_ASSERT(x.arg);
  return x.arg->name();
}

std::string rust_code_generator::operator()(const ast::read_input_struct& x) const {
  WF_ASSERT(x.arg);
  std::string result = x.arg->name();
  for (const access_variant& access : x.access_sequence) {
    overloaded_visit(
        access,
        [&](const field_access& f) {
          fmt::format_to(std::back_inserter(result), ".{}", f.field_name());
        },
        [&](const matrix_access& m) {
          fmt::format_to(std::back_inserter(result), "[({}, {})]", m.row(), m.col());
        });
  }
  return result;
}

std::string rust_code_generator::operator()(const ast::return_value& x) const {
  return operator()(x.value);
}

std::string rust_code_generator::operator()(const ast::special_constant& x) const {
  return std::string{rust_string_for_symbolic_constant(x.value)};
}

std::string rust_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

}  // namespace wf
