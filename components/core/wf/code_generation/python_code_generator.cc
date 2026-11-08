// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/python_code_generator.h"

#include <ranges>
#include <span>
#include <unordered_set>

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"
#include "wf/code_generation/python_backend.h"

namespace wf {

python_code_generator::python_code_generator(python_generator_target target,
                                             python_generator_float_width float_width, int indent,
                                             bool use_output_arguments, bool include_assert_message)
    : target_(target),
      float_width_(float_width),
      use_output_arguments_(use_output_arguments),
      include_assert_message_(include_assert_message),
      indent_(static_cast<std::size_t>(indent)) {
  if (indent < 1) {
    throw wf::invalid_argument_error("Indentation must be >= 1. Provided value: {}", indent);
  }
}

// Format type annotations on scalar arguments.
std::string python_code_generator::operator()(const scalar_type& scalar) const {
  switch (scalar.numeric_type()) {
    case numeric_primitive_type::boolean: {
      return "bool";
    }
    case numeric_primitive_type::integral: {
      return "int";
    }
    case numeric_primitive_type::floating_point: {
      return "float";
    }
  }
  return "<INVALID ENUM VALUE>";
}

std::string python_code_generator::operator()(const matrix_type&) const {
  return std::string{detail::get_python_backend(target_).matrix_type_name()};
}

std::string python_code_generator::operator()(const custom_type& custom) const {
  return custom.name();
}

std::string python_code_generator::operator()(const argument&) const {
  throw type_error("No default formatter for `{}` is provided.", ast::camel_case_name<argument>());
}

std::string python_code_generator::operator()(const ast::function_definition& definition) const {
  const auto& signature = definition.signature();
  std::string result = operator()(signature);
  result.append("\n");

  // Insert some reshape statements on all the input arrays.
  // This ensures we can access with the 2D slice operator.
  for (const auto& arg : signature.arguments()) {
    if (arg.is_matrix()) {
      const auto& mat = std::get<matrix_type>(arg.type());
      if (arg.is_input()) {
        const std::string reshaped =
            detail::get_python_backend(target_).convert_matrix(arg.name(), mat, float_width_);
        fmt::format_to(std::back_inserter(result), "{:{}}{} = {}\n", "", indent_, arg.name(),
                       reshaped);
      }
    } else if (arg.is_scalar()) {
      if (arg.is_input()) {
        const std::string recast = detail::get_python_backend(target_).convert_scalar(
            arg.name(), std::get<scalar_type>(arg.type()).numeric_type(), float_width_);
        fmt::format_to(std::back_inserter(result), "{:{}}{} = {}\n", "", indent_, arg.name(),
                       recast);
      } else if (use_output_arguments_) {
        // TODO: We could support this via 1x1 output numpy array.
        throw wf::type_error(
            "Scalar output arguments are unsupported with the configuration: "
            "use_output_arguments=True (argument name: {})",
            arg.name());
      }
    } else if (arg.is_custom_type() && !arg.is_input() && use_output_arguments_) {
      throw wf::type_error(
          "Custom type output arguments are unsupported with the configuration: "
          "use_output_arguments=True (argument name: {})",
          arg.name());
    }
  }

  std::span<const ast::ast_element> body = definition.body();
  WF_ASSERT(!body.empty());

  const maybe_null<const ast::return_object*> return_statement =
      get_if<ast::return_object>(body.back());
  if (return_statement.has_value()) {
    // Strip the last element before joining, we'll format that below manually.
    body = body.subspan(0, body.size() - 1);
  }
  join_and_indent(result, indent_, "", "\n", "\n", body, *this);

  // Create a return statement that returns a tuple.
  std::vector<std::string> return_tuple_elements;
  return_tuple_elements.reserve(10);
  if (return_statement.has_value()) {
    return_tuple_elements.push_back(operator()(return_statement->value));
  }
  for (const auto& arg : signature.arguments()) {
    if (!arg.is_input() && !use_output_arguments_) {
      return_tuple_elements.emplace_back(arg.name());
    }
  }

  if (!return_tuple_elements.empty()) {
    fmt::format_to(std::back_inserter(result), "{:{}}return ", "", indent_);
    if (return_tuple_elements.size() == 1) {
      result += return_tuple_elements.front();
    } else {
      // Double the indentation - one for the function body, and two for the return statement tuple.
      const std::string close = fmt::format("\n{:{}})", "", indent_);
      join_and_indent(result, indent_ * 2, "(\n", close, ",\n", return_tuple_elements,
                      [](const auto& arg) -> const auto& { return arg; });
    }
  }
  return result;
}

std::string python_code_generator::operator()(const ast::function_signature& signature) const {
  // Outputs are locals even when they are returned rather than passed as arguments.
  std::unordered_set<std::string_view> argument_names;
  for (const auto& arg : signature.arguments()) {
    argument_names.insert(arg.name());
  }
  const auto check_helper_name = [&](const std::string_view name) {
    if (argument_names.count(name)) {
      throw invalid_argument_error(
          "Python argument `{}` conflicts with a generated helper name in function `{}`. "
          "Rename the argument.",
          name, signature.name());
    }
  };
  check_helper_name(detail::get_python_backend(target_).module_alias());
  if (!use_output_arguments_) {
    for (const auto& arg : signature.arguments()) {
      if (arg.is_optional()) {
        check_helper_name(fmt::format("compute_{}", arg.name()));
      }
    }
  }

  std::vector<std::string> args{};
  std::vector<std::string> optional_args{};
  args.reserve(signature.num_arguments());

  for (const auto& arg : signature.arguments()) {
    const auto formatted_type = std::visit(*this, arg.type());
    if (arg.direction() == argument_direction::input) {
      args.push_back(fmt::format("{}: {}", arg.name(), formatted_type));
    } else if (arg.direction() == argument_direction::output) {
      if (use_output_arguments_) {
        args.push_back(fmt::format("{}: {}", arg.name(), formatted_type));
      }
    } else if (arg.direction() == argument_direction::optional_output) {
      if (use_output_arguments_) {
        optional_args.push_back(fmt::format("{}: {} | None = None", arg.name(), formatted_type));
      } else {
        args.push_back(fmt::format("compute_{}: bool", arg.name()));
      }
    }
  }
  // Python requires parameters with defaults to follow all required parameters.
  for (std::string& arg : optional_args) {
    args.push_back(std::move(arg));
  }

  std::vector<std::string> output_arg_ret_annotations;
  if (!use_output_arguments_) {
    output_arg_ret_annotations.reserve(signature.num_arguments());
    for (const auto& arg : signature.arguments()) {
      if (!arg.is_input()) {
        if (arg.is_optional()) {
          output_arg_ret_annotations.push_back(
              fmt::format("{} | None", detail::get_python_backend(target_).matrix_type_name()));
        } else {
          output_arg_ret_annotations.emplace_back(
              detail::get_python_backend(target_).matrix_type_name());
        }
      }
    }
  }

  std::string return_annotation = "None";
  if (signature.return_type()) {
    auto return_type = std::visit(*this, *signature.return_type());
    if (!output_arg_ret_annotations.empty()) {
      return_annotation =
          fmt::format("tuple[{}, {}]", return_type, fmt::join(output_arg_ret_annotations, ", "));
    } else {
      return_annotation = std::move(return_type);
    }
  } else if (!output_arg_ret_annotations.empty()) {
    return_annotation = fmt::format("tuple[{}]", fmt::join(output_arg_ret_annotations, ", "));
  }
  return fmt::format("def {}({}) -> {}:", signature.name(), fmt::join(args, ", "),
                     return_annotation);
}

std::string python_code_generator::operator()(const ast::add& x) const {
  return join(" + ", x.args, *this);
}

std::string python_code_generator::operator()(const ast::assign_output_matrix& x) const {
  if (use_output_arguments_) {
    const matrix_type* mat = std::get_if<matrix_type>(&x.arg.type());
    WF_ASSERT(mat, "Argument {} should be a matrix", x.arg.name());
    return detail::get_python_backend(target_).assign_matrix(x.arg.name(), operator()(x.value));
  } else {
    return fmt::format("{} = {}", x.arg.name(), operator()(x.value));
  }
}

std::string python_code_generator::operator()(const ast::assign_output_scalar& x) const {
  return fmt::format("{} = {}", x.arg.name(), make_view(x.value));
}

std::string python_code_generator::operator()(const ast::assign_output_struct& x) const {
  return fmt::format("{} = {}", x.arg.name(), make_view(x.value));
}

std::string python_code_generator::operator()(const ast::assign_temporary& x) const {
  return fmt::format("{} = {}", x.left, make_view(x.right));
}

std::string python_code_generator::operator()(const ast::boolean_literal& x) const {
  return x.value ? "True" : "False";
}

std::string python_code_generator::operator()(const ast::branch& x) const {
  std::string result{};
  fmt::format_to(std::back_inserter(result), "if {}", make_view(x.condition));
  join_and_indent(result, indent_, ":\n", "\n", "\n", x.if_branch, *this);
  if (x.else_branch.size() > 0) {
    result.append("else");
    join_and_indent(result, indent_, ":\n", "", "\n", x.else_branch, *this);
  }
  return result;
}

std::string python_code_generator::operator()(const ast::call_external_function& x) const {
  const std::string result = fmt::format("{}({})", x.function.name(),
                                         fmt::join(x.args | std::views::transform(*this), ", "));
  if (const matrix_type* mat = std::get_if<matrix_type>(&x.function.return_type());
      mat != nullptr) {
    return detail::get_python_backend(target_).convert_matrix(result, *mat, float_width_);
  }
  return result;
}

std::string python_code_generator::operator()(const ast::call_std_function& x) const {
  const auto args = transform_map<std::vector>(x.args, *this);
  return detail::get_python_backend(target_).call_math(x.function, args, float_width_);
}

std::string python_code_generator::operator()(const ast::cast& x) const {
  return detail::get_python_backend(target_).convert_scalar(operator()(x.arg), x.destination_type,
                                                            float_width_);
}

std::string python_code_generator::operator()(const ast::comment& x) const {
  return join("\n", x.split_lines(),
              [](const std::string& line) { return fmt::format("# {}", line); });
}

std::string python_code_generator::operator()(const ast::compare& x) const {
  return fmt::format("{} {} {}", make_view(x.left), string_from_relational_operation(x.operation),
                     make_view(x.right));
}

std::string python_code_generator::operator()(const ast::construct_matrix& x) const {
  const auto args = transform_map<std::vector>(x.args, *this);
  return detail::get_python_backend(target_).construct_matrix(args, x.type, float_width_);
}

std::string python_code_generator::operator()(const ast::construct_custom_type& x) const {
  std::string result = operator()(x.type);
  join_and_indent(result, indent_, "(\n", "\n)", ",\n", x.field_values,
                  [&](const auto& name_and_val) {
                    return fmt::format("{}={}", std::get<0>(name_and_val),
                                       make_view(std::get<1>(name_and_val)));
                  });
  return result;
}

std::string python_code_generator::operator()(const ast::declaration& x) const {
  if (x.value) {
    return fmt::format("{} = {}", x.name, make_view(*x.value));
  } else {
    return fmt::format("{} = None", x.name);
  }
}

std::string python_code_generator::operator()(const ast::divide& x) const {
  return fmt::format("{} / {}", make_view(x.left), make_view(x.right));
}

std::string python_code_generator::operator()(const ast::float_literal& x) const {
  return detail::get_python_backend(target_).floating_literal(fmt::format("{}", x.value),
                                                              float_width_);
}

std::string python_code_generator::operator()(const ast::get_argument& x) const {
  return x.arg.name();
}

std::string python_code_generator::operator()(const ast::get_field& x) const {
  return fmt::format("{}.{}", make_view(x.arg), x.field);
}

std::string python_code_generator::operator()(const ast::get_matrix_element& x) const {
  return fmt::format("{}[{}, {}]", make_view(x.arg), x.row, x.col);
}

std::string python_code_generator::operator()(const ast::integer_literal& x) const {
  return fmt::format("{}", x.value);
}

std::string python_code_generator::operator()(const ast::multiply& x) const {
  return join(" * ", x.args, *this);
}

std::string python_code_generator::operator()(const ast::negate& x) const {
  return fmt::format("-{}", make_view(x.arg));
}

std::string python_code_generator::operator()(const ast::optional_output_branch& x) const {
  std::string result;
  if (use_output_arguments_) {
    fmt::format_to(std::back_inserter(result), "if {} is not None:", x.arg.name());
    if (const matrix_type* mat = std::get_if<matrix_type>(&x.arg.type()); mat != nullptr) {
      const auto size = detail::get_python_backend(target_).matrix_size(x.arg.name());
      fmt::format_to(std::back_inserter(result), "\n{:{}}assert {} == {}", "", indent_, size,
                     mat->size());
      if (include_assert_message_) {
        fmt::format_to(std::back_inserter(result),
                       ", f\"Matrix {} should have {} elements, but it has {{{}}}\"", x.arg.name(),
                       mat->size(), size);
      }
    }
  } else {
    fmt::format_to(std::back_inserter(result), "if compute_{}:", x.arg.name());
  }
  join_and_indent(result, indent_, "\n", !use_output_arguments_ ? "\n" : "", "\n", x.statements,
                  *this);
  if (!use_output_arguments_) {
    fmt::format_to(std::back_inserter(result), "else:\n{:{}}{} = None", "", indent_, x.arg.name());
  }
  return result;
}

std::string python_code_generator::operator()(const ast::parenthetical& x) const {
  return fmt::format("({})", make_view(x.contents));
}

std::string python_code_generator::operator()(const ast::return_object&) const {
  throw type_error("No default formatter for `{}` is provided.",
                   ast::camel_case_name<ast::return_object>());
}

std::string python_code_generator::operator()(const ast::special_constant& x) const {
  return detail::get_python_backend(target_).symbolic_constant(x.value, float_width_);
}

std::string python_code_generator::operator()(const ast::ternary& x) const {
  return detail::get_python_backend(target_)
      .select(operator()(x.condition), operator()(x.left), operator()(x.right));
}

std::string python_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

std::string python_code_generator::operator()(const ast::ast_element& element) const {
  return ast::visit(element, *this);
}

inline constexpr std::string_view preamble = R"code(# Machine generated code.
import typing as T
{module_import}

{code}
)code";

std::string python_code_generator::apply_preamble(const std::string_view code) const {
  WF_ASSERT(code.data());
  return fmt::format(preamble,
                     fmt::arg("module_import", detail::get_python_backend(target_).module_import()),
                     fmt::arg("code", code));
}

}  // namespace wf
