// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/cpp_code_generator.h"

#include <iterator>
#include <ranges>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/ast_visitor.h"
#include "wf/code_generation/function_description.h"
#include "wf/code_generation/types.h"
#include "wf/utility/assertions.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {

static constexpr std::string_view utility_namespace = "wf";

constexpr static std::string_view cpp_string_from_numeric_type(
    const numeric_primitive_type destination_type) noexcept {
  switch (destination_type) {
    case numeric_primitive_type::boolean:
      return "bool";
    case numeric_primitive_type::integral:
      return "std::int64_t";
    case numeric_primitive_type::floating_point:
      return "Scalar";
  }
  return "<INVALID ENUM VALUE>";
}

template <typename T>
[[noreturn]] void throw_matrix_error() {
  throw type_error(
      "The default C++ code-generator treats all matrices as spans. You probably want to either:\n"
      "* Pass CppMatrixTypeArgument.Eigen to CppCodeGenerator on construction, or:\n"
      "* Implement a formatter override for the `{}` type.\n"
      "See: https://wrenfold.org/reference/returning_matrices",
      T::snake_case_name_str);
}

std::string cpp_code_generator::operator()(const matrix_type& mat) const {
  if (behavior_ == cpp_matrix_type_behavior::eigen) {
    // This is invoked when formatting return types and declarations inside functions.
    // We declare a standard eigen matrix.
    return fmt::format("Eigen::Matrix<Scalar, {}, {}>", mat.rows(), mat.cols());
  } else {
    throw_matrix_error<matrix_type>();
  }
}

std::string cpp_code_generator::operator()(const scalar_type& scalar) const {
  return std::string{cpp_string_from_numeric_type(scalar.numeric_type())};
}

std::string cpp_code_generator::operator()(const custom_type& custom) const {
  return custom.name();
}

std::string cpp_code_generator::operator()(const argument& arg) const {
  std::string result;
  overloaded_visit(
      arg.type(),
      [&](const scalar_type s) {
        if (arg.direction() == argument_direction::input) {
          fmt::format_to(std::back_inserter(result), "const {}", make_view(s));
        } else if (arg.direction() == argument_direction::output) {
          fmt::format_to(std::back_inserter(result), "{}&", make_view(s));
        } else {
          // TODO: Fix the output type here to be a span.
          fmt::format_to(std::back_inserter(result), "{}*", make_view(s));
        }
      },
      [&](const matrix_type mat) {
        if (behavior_ == cpp_matrix_type_behavior::generic_span) {
          if (arg.direction() == argument_direction::input) {
            fmt::format_to(std::back_inserter(result), "const T{}&", arg.index());
          } else {
            fmt::format_to(std::back_inserter(result), "T{}&&", arg.index());
          }
        } else {
          if (arg.direction() == argument_direction::input) {
            fmt::format_to(std::back_inserter(result), "const Eigen::Matrix<Scalar, {}, {}>&",
                           mat.rows(), mat.cols());
          } else if (arg.direction() == argument_direction::output) {
            fmt::format_to(std::back_inserter(result), "Eigen::Matrix<Scalar, {}, {}>&", mat.rows(),
                           mat.cols());
          } else if (arg.direction() == argument_direction::optional_output) {
            fmt::format_to(std::back_inserter(result), "Eigen::Matrix<Scalar, {}, {}>* const",
                           mat.rows(), mat.cols());
          }
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
  result.append(" ");
  result.append(arg.name());
  return result;
}

std::string cpp_code_generator::operator()(const ast::function_definition& definition) const {
  std::string result = operator()(definition.signature());
  result.append("\n{\n");

  if (behavior_ == cpp_matrix_type_behavior::generic_span) {
    const auto& arguments = definition.signature().arguments();
    std::vector<argument> matrix_args{};
    std::copy_if(arguments.begin(), arguments.end(), std::back_inserter(matrix_args),
                 [](const auto& arg) { return arg.is_matrix(); });

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
  }

  join_and_indent(result, 2, "", "\n}", "\n", definition.body(), *this);
  return result;
}

std::string cpp_code_generator::operator()(const ast::function_signature& signature) const {
  // Template parameter list:
  std::string result = "template <typename Scalar";
  if (signature.has_matrix_arguments() && behavior_ == cpp_matrix_type_behavior::generic_span) {
    for (const argument& arg : signature.arguments()) {
      if (arg.is_matrix()) {
        fmt::format_to(std::back_inserter(result), ", typename T{}", arg.index());
      }
    }
  }
  result.append(">\n");

  // Return type and name:
  if (const auto& maybe_return_type = signature.return_type(); maybe_return_type.has_value()) {
    result += std::visit(*this, *maybe_return_type);
    result.append(" ");
  } else {
    result.append("void ");
  }

  result.append(signature.name());
  result.append("(");
  result += join(", ", signature.arguments(), *this);
  result.append(")");
  return result;
}

std::string cpp_code_generator::operator()(const ast::add& x) const {
  return join(" + ", x.args, *this);
}

std::string cpp_code_generator::operator()(const ast::assign_output_matrix& x) const {
  return join("\n", std::ranges::iota_view{static_cast<std::size_t>(0), x.value.type.size()},
              [&](const std::size_t i) {
                const auto [row, col] = x.value.type.compute_indices(i);
                if (behavior_ == cpp_matrix_type_behavior::generic_span) {
                  return fmt::format("_{}({}, {}) = {};", x.arg.name(), row, col,
                                     make_view(x.value.args[i]));
                } else {
                  if (x.arg.is_optional()) {
                    return fmt::format("{}->operator()({}, {}) = {};", x.arg.name(), row, col,
                                       make_view(x.value.args[i]));
                  } else {
                    return fmt::format("{}({}, {}) = {};", x.arg.name(), row, col,
                                       make_view(x.value.args[i]));
                  }
                }
              });
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

std::string cpp_code_generator::operator()(const ast::boolean_literal& x) const {
  return x.value ? "true" : "false";
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
  const std::string args = join(", ", x.args, *this);
  if (const scalar_type* s = std::get_if<scalar_type>(&x.function.return_type());
      static_cast<bool>(s) && s->numeric_type() == numeric_primitive_type::floating_point) {
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
    case std_math_function::cosh:
    case std_math_function::sinh:
    case std_math_function::tanh:
    case std_math_function::acosh:
    case std_math_function::asinh:
    case std_math_function::atanh:
    case std_math_function::log:
    case std_math_function::exp:
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
                         cpp_string_from_numeric_type(numeric_primitive_type::integral),
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

std::string cpp_code_generator::operator()(const ast::construct_matrix& x) const {
  if (behavior_ == cpp_matrix_type_behavior::generic_span) {
    throw_matrix_error<ast::construct_matrix>();
  } else {
    return fmt::format("(Eigen::Matrix<Scalar, {}, {}>() << {}).finished()", x.type.rows(),
                       x.type.cols(), fmt::join(x.args | std::views::transform(*this), ", "));
  }
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
  output += std::visit(*this, x.type);
  if (x.value) {
    fmt::format_to(std::back_inserter(output), " {} = {};", x.name, make_view(*x.value));
  } else {
    fmt::format_to(std::back_inserter(output), " {};", x.name);
  }
  return output;
}

std::string cpp_code_generator::operator()(const ast::divide& x) const {
  return fmt::format("{} / {}", make_view(x.left), make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::float_literal& x) const {
  return fmt::format("static_cast<Scalar>({})", x.value);
}

std::string cpp_code_generator::operator()(const ast::get_argument& x) const {
  if (x.arg.is_matrix() && behavior_ == cpp_matrix_type_behavior::generic_span) {
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
  }
  return "<INVALID ENUM VALUE>";
}

std::string cpp_code_generator::operator()(const ast::multiply& x) const {
  return join(" * ", x.args, *this);
}

std::string cpp_code_generator::operator()(const ast::negate& x) const {
  return fmt::format("-{}", make_view(x.arg));
}

std::string cpp_code_generator::operator()(const ast::optional_output_branch& x) const {
  std::string result{};
  const std::string_view arg_name_prefix =
      (x.arg.is_matrix() && behavior_ == cpp_matrix_type_behavior::generic_span) ? "_" : "";
  fmt::format_to(std::back_inserter(result), "if (static_cast<bool>({}{})) ", arg_name_prefix,
                 x.arg.name());
  join_and_indent(result, 2, "{\n", "\n}", "\n", x.statements, *this);
  return result;
}

std::string cpp_code_generator::operator()(const ast::parenthetical& x) const {
  return fmt::format("({})", make_view(x.contents));
}

std::string cpp_code_generator::operator()(const ast::return_object& x) const {
  return fmt::format("return {};", make_view(x.value));
}

std::string cpp_code_generator::operator()(const ast::special_constant& x) const {
  return fmt::format("static_cast<Scalar>({})", cpp_string_for_symbolic_constant(x.value));
}

std::string cpp_code_generator::operator()(const ast::ternary& x) const {
  return fmt::format("{} ? {} : {}", make_view(x.condition), make_view(x.left), make_view(x.right));
}

std::string cpp_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

std::string cpp_code_generator::operator()(const ast::ast_element& element) const {
  return ast::visit(element, *this);
}

inline constexpr std::string_view preamble = R"code(// Machine generated code.
#pragma once
#include <cmath>
#include <cstdint>

{runtime_import}

{imports}
namespace {namespace} {{

{code}

}} // namespace {namespace})code";

std::string cpp_code_generator::apply_preamble(const std::string_view code,
                                               const std::string_view ns,
                                               const std::string_view imports) const {
  WF_ASSERT(code.data());
  WF_ASSERT(ns.data());
  WF_ASSERT(imports.data());
  const std::string imports_formatted =
      imports.size() > 0 ? fmt::format("// User-specified imports:\n{}\n\n", imports) : "";
  const std::string_view runtime_import = behavior_ == cpp_matrix_type_behavior::generic_span
                                              ? "#include <wrenfold/span.h>"
                                              : "#include <Eigen/Core>";
  return fmt::format(preamble, fmt::arg("code", code), fmt::arg("runtime_import", runtime_import),
                     fmt::arg("namespace", ns), fmt::arg("imports", imports_formatted));
}

}  // namespace wf
