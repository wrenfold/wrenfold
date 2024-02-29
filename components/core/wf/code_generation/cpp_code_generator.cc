// Copyright 2023 Gareth Cross
#include "wf/code_generation/cpp_code_generator.h"

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"
#include "wf/index_range.h"
#include "wf/template_utils.h"

namespace wf {

static constexpr std::string_view utility_namespace = "wf";

constexpr static std::string_view cpp_string_from_numeric_type(
    const code_numeric_type destination_type) noexcept {
  switch (destination_type) {
    case code_numeric_type::boolean:
      return "bool";
    case code_numeric_type::integral:
      return "std::int64_t";
    case code_numeric_type::floating_point:
      return "Scalar";
  }
  return "<INVALID ENUM VALUE>";
}

constexpr static std::string_view cpp_string_from_numeric_type(
    const scalar_type& destination_type) noexcept {
  return cpp_string_from_numeric_type(destination_type.numeric_type());
}

std::string cpp_code_generator::operator()(const argument& arg) const {
  std::string result{};
  overloaded_visit(
      arg.type(),
      [&](const scalar_type s) {
        if (arg.direction() == argument_direction::input) {
          fmt::format_to(std::back_inserter(result), "const {}", cpp_string_from_numeric_type(s));
        } else if (arg.direction() == argument_direction::output) {
          fmt::format_to(std::back_inserter(result), "{}&", cpp_string_from_numeric_type(s));
        } else {
          // TODO: Fix the output type here to be a span.
          fmt::format_to(std::back_inserter(result), "{}*", cpp_string_from_numeric_type(s));
        }
      },
      [&](matrix_type) {
        if (arg.direction() == argument_direction::input) {
          fmt::format_to(std::back_inserter(result), "const T{}&", arg.index());
        } else {
          fmt::format_to(std::back_inserter(result), "T{}&&", arg.index());
        }
      },
      [&](const custom_type& custom) {
        if (arg.direction() == argument_direction::input) {
          fmt::format_to(std::back_inserter(result), "const {}&", make_view(custom));
        } else if (arg.direction() == argument_direction::output) {
          fmt::format_to(std::back_inserter(result), "{}&", make_view(custom));
        } else if (arg.direction() == argument_direction::optional_output) {
          fmt::format_to(std::back_inserter(result), "{}*", make_view(custom));
        }
      });

  fmt::format_to(std::back_inserter(result), " {}", arg.name());
  return result;
}

std::string cpp_code_generator::operator()(const custom_type& custom) const {
  return custom.name();
}

std::string cpp_code_generator::operator()(const ast::function_definition& definition) const {
  std::string result = operator()(definition.signature());
  result.append("\n{\n");

  std::vector<argument> matrix_args{};
  std::copy_if(definition.signature().arguments().begin(), definition.signature().arguments().end(),
               std::back_inserter(matrix_args), [](const auto& arg) { return arg.is_matrix(); });

  if (!matrix_args.empty()) {
    join_and_indent(result, 2, "", "\n", "\n", matrix_args, [](const argument& arg) {
      const matrix_type& mat = std::get<matrix_type>(arg.type());

      // Generate matrix conversion logic.
      std::string arg_result;
      fmt::format_to(std::back_inserter(arg_result), "auto _{} = ", arg.name());

      const std::string dims_type = fmt::format("{}, {}", mat.rows(), mat.cols());
      switch (arg.direction()) {
        case argument_direction::input:
          fmt::format_to(std::back_inserter(arg_result), "{}::make_input_span<{}>({});",
                         utility_namespace, dims_type, arg.name());
          break;
        case argument_direction::output:
          fmt::format_to(std::back_inserter(arg_result), "{}::make_output_span<{}>({});",
                         utility_namespace, dims_type, arg.name());
          break;
        case argument_direction::optional_output:
          fmt::format_to(std::back_inserter(arg_result), "{}::make_optional_output_span<{}>({});",
                         utility_namespace, dims_type, arg.name());
          break;
      }
      return arg_result;
    });
    result.append("\n");
  }

  join_and_indent(result, 2, "", "\n}", "\n", definition.body(), *this);
  return result;
}

std::string cpp_code_generator::operator()(const ast::function_signature& signature) const {
  // Template parameter list:
  std::string result = "template <typename Scalar";
  if (signature.has_matrix_arguments()) {
    for (const argument& arg : signature.arguments()) {
      if (arg.is_matrix()) {
        fmt::format_to(std::back_inserter(result), ", typename T{}", arg.index());
      }
    }
  }
  result.append(">\n");

  // Return type and name:
  fmt::format_to(std::back_inserter(result), "{} {}(", make_view(signature.return_annotation()),
                 signature.name());
  result += join(*this, ", ", signature.arguments());
  result.append(")");
  return result;
}

std::string cpp_code_generator::operator()(const ast::return_type_annotation& x) const {
  if (x.type) {
    return overloaded_visit(
        x.type.value(), [&](scalar_type) { return std::string{"Scalar"}; },
        [&](matrix_type) -> std::string {
          throw type_error(
              "The default C++ code-generator treats all matrices as spans. We cannot return one "
              "directly. You likely want to implement an override for the {} ast type.",
              ast::return_type_annotation::snake_case_name_str);
        },
        [this](const custom_type& custom_type) { return operator()(custom_type); });
  } else {
    return "void";
  }
}

std::string cpp_code_generator::operator()(const ast::add& x) const {
  return fmt::format("{} + {}", make_view(x.left), make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::assign_output_matrix& x) const {
  const auto range = make_range(static_cast<std::size_t>(0), x.value.type.size());
  return join(
      [&](const std::size_t i) {
        const auto [row, col] = x.value.type.compute_indices(i);
        return fmt::format("_{}({}, {}) = {};", x.arg.name(), row, col, make_view(x.value.args[i]));
      },
      "\n", range);
}

std::string cpp_code_generator::operator()(const ast::assign_output_scalar& x) const {
  if (x.arg.is_optional()) {
    return fmt::format("*{} = {};", x.arg.name(), make_view(x.value));
  } else {
    return fmt::format("{} = {};", x.arg.name(), make_view(x.value));
  }
}

std::string cpp_code_generator::operator()(const ast::assign_output_struct& x) const {
  if (x.arg.is_optional()) {
    return fmt::format("*{} = {};", x.arg.name(), make_view(x.value));
  } else {
    return fmt::format("{} = {};", x.arg.name(), make_view(x.value));
  }
}

std::string cpp_code_generator::operator()(const ast::assign_temporary& x) const {
  return fmt::format("{} = {};", x.left, make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::branch& x) const {
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if ({}) ", make_view(x.condition));
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.if_branch, *this);
  if (!x.else_branch.empty()) {
    join_and_indent(result, 2, " else {\n", "\n}", "\n", x.else_branch, *this);
  }
  return result;
}

std::string cpp_code_generator::operator()(const ast::call_external_function& x) const {
  const std::string args = join(*this, ", ", x.args);
  if (const scalar_type* s = std::get_if<scalar_type>(&x.function.return_type());
      static_cast<bool>(s) && s->numeric_type() == code_numeric_type::floating_point) {
    return fmt::format("static_cast<Scalar>({}({}))", x.function.name(), args);
  } else {
    return fmt::format("{}({})", x.function.name(), args);
  }
}

std::string cpp_code_generator::operator()(const ast::call_std_function& x) const {
  switch (x.function) {
    case std_math_function::cos:
    case std_math_function::sin:
    case std_math_function::tan:
    case std_math_function::acos:
    case std_math_function::asin:
    case std_math_function::atan:
    case std_math_function::log:
    case std_math_function::sqrt:
    case std_math_function::abs:
      return fmt::format("std::{}({})", string_from_standard_library_function(x.function),
                         make_view(x[0]));
    case std_math_function::signum:
      // TODO: Casts should be different, depending on type of first arg.
      return fmt::format("(static_cast<Scalar>(0) < {arg}) - ({arg} < static_cast<Scalar>(0))",
                         fmt::arg("arg", make_view(x[0])));
    case std_math_function::floor:
      return fmt::format("static_cast<{}>(std::floor({}))",
                         cpp_string_from_numeric_type(code_numeric_type::integral),
                         make_view(x[0]));
    case std_math_function::atan2:
      return fmt::format("std::atan2({}, {})", make_view(x[0]), make_view(x[1]));
    case std_math_function::powi:
    case std_math_function::powf:
      return fmt::format("std::pow({}, {})", make_view(x[0]), make_view(x[1]));
    default:
      break;
  }
  WF_ASSERT_ALWAYS("Unhandled standard math function: {}",
                   string_from_standard_library_function(x.function));
}

std::string cpp_code_generator::operator()(const ast::cast& x) const {
  return fmt::format("static_cast<{}>({})", cpp_string_from_numeric_type(x.destination_type),
                     make_view(x.arg));
}

std::string cpp_code_generator::operator()(const ast::comment& x) const {
  const std::vector<std::string> lines = x.split_lines();
  std::string result{};
  for (const auto& line : lines) {
    fmt::format_to(std::back_inserter(result), "// {}\n", line);
  }
  return result;
}

std::string cpp_code_generator::operator()(const ast::compare& x) const {
  return fmt::format("{} {} {}", make_view(x.left), string_from_relational_operation(x.operation),
                     make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::construct_matrix&) const {
  throw type_error(
      "The default C++ code-generator treats all matrices as spans. We cannot construct one "
      "directly. You likely want to implement an override for the the `{}` ast type.",
      ast::construct_matrix::snake_case_name_str);
}

// Really we don't know how the user wants their types constructed, but we can take an educated
// guess. Customization is possible from python via overrides.
std::string cpp_code_generator::operator()(const ast::construct_custom_type& x) const {
  const std::string opener = fmt::format("{}{{\n", make_view(x.type));
  std::string output{};
  join_and_indent(output, 2, opener, "\n}", ",\n", x.field_values, [this](const auto& field_val) {
    const auto& [_, val] = field_val;
    return operator()(val);
  });
  return output;
}

std::string cpp_code_generator::operator()(const ast::declaration& x) const {
  std::string output;
  if (x.value) {
    output.append("const ");
  }
  output += operator()(x.type);  //  Delegate for the type itself.
  if (x.value) {
    fmt::format_to(std::back_inserter(output), " {} = {};", x.name, make_view(*x.value));
  } else {
    fmt::format_to(std::back_inserter(output), " {};", x.name);
  }
  return output;
}

std::string cpp_code_generator::operator()(const ast::declaration_type_annotation& x) const {
  return overloaded_visit(
      x.type,
      [](const scalar_type s) -> std::string {
        return std::string(cpp_string_from_numeric_type(s));
      },
      [](const matrix_type&) -> std::string {
        throw type_error(
            "The default C++ code-generator treats all matrices as spans. We cannot construct one "
            "directly. You likely want to implement an override for the the `{}` ast type.",
            ast::declaration_type_annotation::snake_case_name_str);
      },
      [](const custom_type& c) -> std::string {
        // TODO: Should be const-ref if we have an rhs - that way we avoid copies when calling
        //  an external function.
        return c.name();
      });
}

std::string cpp_code_generator::operator()(const ast::divide& x) const {
  return fmt::format("{} / {}", make_view(x.left), make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::float_literal& x) const {
  return fmt::format("static_cast<Scalar>({})", x.value);
}

std::string cpp_code_generator::operator()(const ast::get_argument& x) const {
  if (x.arg.is_matrix()) {
    // Access the span indirection we created.
    return fmt::format("_{}", x.arg.name());
  } else {
    return x.arg.name();
  }
}

std::string cpp_code_generator::operator()(const ast::get_field& x) const {
  return fmt::format("{}.{}", make_view(x.arg), x.field);
}

std::string cpp_code_generator::operator()(const ast::get_matrix_element& x) const {
  return fmt::format("{}({}, {})", make_view(x.arg), x.row, x.col);
}

std::string cpp_code_generator::operator()(const ast::integer_literal& x) const {
  return fmt::format("{}", x.value);
}

static constexpr std::string_view cpp_string_for_symbolic_constant(
    const symbolic_constant_enum value) noexcept {
  switch (value) {
    case symbolic_constant_enum::euler:
      return "M_E";
    case symbolic_constant_enum::pi:
      return "M_PI";
    case symbolic_constant_enum::boolean_true:
      return "true";
    case symbolic_constant_enum::boolean_false:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

std::string cpp_code_generator::operator()(const ast::multiply& x) const {
  return fmt::format("{} * {}", make_view(x.left), make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::negate& x) const {
  return fmt::format("-{}", make_view(x.arg));
}

std::string cpp_code_generator::operator()(const ast::optional_output_branch& x) const {
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if (static_cast<bool>({}{})) ",
                 x.arg.is_matrix() ? "_" : "", x.arg.name());
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.statements, *this);
  return result;
}

std::string cpp_code_generator::operator()(const ast::return_object& x) const {
  return fmt::format("return {};", make_view(x.value));
}

std::string cpp_code_generator::operator()(const ast::special_constant& x) const {
  return fmt::format("static_cast<Scalar>({})", cpp_string_for_symbolic_constant(x.value));
}

std::string cpp_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

std::string cpp_code_generator::operator()(const ast::ast_element& element) const {
  return ast::visit(element, *this);
}

}  // namespace wf
