// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/python_code_generator.h"

#include <ranges>
#include <span>

#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ast_visitor.h"

namespace wf {

constexpr std::string_view python_string_from_float_width(
    python_generator_float_width width) noexcept {
  switch (width) {
    case python_generator_float_width::float32: {
      return "float32";
    }
    case python_generator_float_width::float64: {
      return "float64";
    }
  }
  return "<INVALID ENUM VALUE>";
}

constexpr static std::string_view python_string_from_cast_destination_type(
    const numeric_primitive_type destination_type,
    const python_generator_float_width float_width) noexcept {
  switch (destination_type) {
    case numeric_primitive_type::boolean:
      return "bool";
    case numeric_primitive_type::integral:
      // Int32 because JAX does not support 64 by default. TODO: add a parameter for this.
      return "int32";
    case numeric_primitive_type::floating_point: {
      return python_string_from_float_width(float_width);
    }
  }
  return "<INVALID ENUM VALUE>";
}

constexpr static std::string_view python_module_prefix_from_target(
    const python_generator_target target) noexcept {
  switch (target) {
    case python_generator_target::numpy: {
      return "np";
    }
    case python_generator_target::pytorch: {
      return "th";
    }
    case python_generator_target::jax: {
      return "jnp";
    }
  }
  return "<INVALID ENUM VALUE>";
}

inline std::string format_asarray(const python_generator_target target, const std::string_view arg,
                                  const numeric_primitive_type expected_type,
                                  const python_generator_float_width float_width,
                                  const index_t rows, const index_t cols) {
  return fmt::format(
      "{}.asarray({}, dtype={}.{}).reshape([{}, {}])", python_module_prefix_from_target(target),
      arg, python_module_prefix_from_target(target),
      python_string_from_cast_destination_type(expected_type, float_width), rows, cols);
}

python_code_generator::python_code_generator(python_generator_target target,
                                             python_generator_float_width float_width, int indent)
    : target_(target), float_width_(float_width), indent_(static_cast<std::size_t>(indent)) {
  if (indent < 1) {
    throw wf::invalid_argument_error("Indentation must be >= 1. Provided value: {}", indent);
  }
}

constexpr std::string_view python_matrix_type_from_target(python_generator_target target) noexcept {
  switch (target) {
    case python_generator_target::numpy: {
      return "np.ndarray";
    }
    case python_generator_target::pytorch: {
      return "th.Tensor";
    }
    case python_generator_target::jax: {
      return "jnp.ndarray";
    }
  }
  return "<INVALID ENUM VALUE>";
}

std::string python_code_generator::operator()(const scalar_type& scalar) const {
  // Arguments can be passed as scalars, or as single-element arrays/tensors.
  // So we annotate with either type.
  // Once we drop Python 3.9 support, this can be a union written like: `X | Y`
  // See: https://peps.python.org/pep-0604/
  const auto matrix_alternative = python_matrix_type_from_target(target_);
  if (target_ == python_generator_target::numpy) {
    // Numpy treats float32, float64, bool, int64 as specific types.
    // So the input could be a python primitive, a numpy primitive type, or a single element array.
    switch (scalar.numeric_type()) {
      case numeric_primitive_type::boolean: {
        return fmt::format("T.Union[np.bool, {}]", matrix_alternative);
      }
      case numeric_primitive_type::integral: {
        return fmt::format("T.Union[np.int64, {}]", matrix_alternative);
      }
      case numeric_primitive_type::floating_point: {
        return fmt::format("T.Union[np.{}, {}]", python_string_from_float_width(float_width_),
                           matrix_alternative);
      }
    }
  }
  return std::string(matrix_alternative);
}

std::string python_code_generator::operator()(const matrix_type&) const {
  return std::string{python_matrix_type_from_target(target_)};
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
  // This ensures we can access with the 2D slice operator (and validates shapes).
  for (const auto& arg : signature.arguments()) {
    if (arg.is_input() && arg.is_matrix()) {
      const auto& mat = std::get<matrix_type>(arg.type());
      const std::string reshaped =
          format_asarray(target_, arg.name(), numeric_primitive_type::floating_point, float_width_,
                         mat.rows(), mat.cols());
      fmt::format_to(std::back_inserter(result), "{:{}}{} = {}\n", "", indent_, arg.name(),
                     reshaped);
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

  // Create a list of all "output arguments" (which are here converted to returned members of a
  // dict).
  std::vector<std::string_view> output_args{};
  output_args.reserve(signature.num_arguments());
  for (const auto& arg : signature.arguments()) {
    if (!arg.is_input()) {
      output_args.emplace_back(arg.name());
    }
  }

  // Create a return statement that yields:
  // - The return value
  // - A dict of "output args"
  // Either one of these, or both as a tuple.
  std::vector<std::string> return_tuple_elements;
  return_tuple_elements.reserve(2);

  if (return_statement.has_value()) {
    return_tuple_elements.push_back(operator()(return_statement->value));
  }
  if (!output_args.empty()) {
    const std::string dict_args = join(", ", output_args, [](const std::string_view arg) {
      return fmt::format("{}={}", arg, arg);
    });
    return_tuple_elements.push_back(fmt::format("dict({})", dict_args));
  }

  if (!return_tuple_elements.empty()) {
    fmt::format_to(std::back_inserter(result), "{:{}}return ", "", indent_);
    if (return_tuple_elements.size() == 1) {
      result += return_tuple_elements.front();
    } else if (return_tuple_elements.size() == 2) {
      // Double the indentation - one for the function body, and two for the return statement tuple.
      const std::string close = fmt::format("\n{:{}})", "", indent_);
      join_and_indent(result, indent_ * 2, "(\n", close, ",\n", return_tuple_elements,
                      [](const auto& arg) -> const auto& { return arg; });
    }
  }
  return result;
}

std::string python_code_generator::operator()(const ast::function_signature& signature) const {
  std::vector<std::string> args{};
  args.reserve(signature.num_arguments());

  for (const auto& arg : signature.arguments()) {
    if (arg.direction() == argument_direction::input) {
      const auto formatted_type = std::visit(*this, arg.type());
      args.push_back(fmt::format("{}: {}", arg.name(), formatted_type));
    } else if (arg.direction() == argument_direction::optional_output) {
      args.push_back(fmt::format("compute_{}: bool", arg.name()));
    }
  }

  const bool has_output_args = any_of(signature.arguments(), [](const argument& a) {
    return a.direction() != argument_direction::input;
  });

  std::string return_annotation = "None";
  if (signature.return_type()) {
    auto return_type = std::visit(*this, *signature.return_type());
    if (has_output_args) {
      return_annotation = fmt::format("T.Tuple[{}, T.Dict[str, {}]]", return_type,
                                      python_matrix_type_from_target(target_));
    } else {
      return_annotation = std::move(return_type);
    }
  } else if (has_output_args) {
    return_annotation = fmt::format("T.Dict[str, {}]", python_matrix_type_from_target(target_));
  }
  return fmt::format("def {}({}) -> {}:", signature.name(), fmt::join(args, ", "),
                     return_annotation);
}

std::string python_code_generator::operator()(const ast::add& x) const {
  return join(" + ", x.args, *this);
}

constexpr static std::string_view python_matrix_constructor_from_target(
    const python_generator_target target) noexcept {
  switch (target) {
    case python_generator_target::numpy: {
      return "np.array";
    }
    case python_generator_target::pytorch: {
      // We use `stack` here because it works with `th.vmap`.
      return "th.stack";
    }
    case python_generator_target::jax: {
      return "jnp.array";
    }
  }
  return "<INVALID ENUM VALUE>";
}

std::string python_code_generator::operator()(const ast::assign_output_matrix& x) const {
  return fmt::format("{} = {}", x.arg.name(), operator()(x.value));
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
    join_and_indent(result, indent_, ":\n", "\n", "\n", x.else_branch, *this);
  }
  return result;
}

std::string python_code_generator::operator()(const ast::call_external_function& x) const {
  const std::string result = fmt::format("{}({})", x.function.name(),
                                         fmt::join(x.args | std::views::transform(*this), ", "));
  if (const matrix_type* mat = std::get_if<matrix_type>(&x.function.return_type());
      mat != nullptr) {
    return fmt::format("{}.asarray({}, dtype={}.{}).reshape([{}, {}])",
                       python_module_prefix_from_target(target_), result,
                       python_module_prefix_from_target(target_),
                       python_string_from_cast_destination_type(
                           numeric_primitive_type::floating_point, float_width_),
                       mat->rows(), mat->cols());
  }
  return result;
}

constexpr static std::string_view python_string_from_std_math_function(
    const std_math_function function) {
  switch (function) {
    case std_math_function::cos:
    case std_math_function::sin:
    case std_math_function::tan:
    case std_math_function::cosh:
    case std_math_function::sinh:
    case std_math_function::tanh:
    case std_math_function::sqrt:
    case std_math_function::abs:
    case std_math_function::floor:
    case std_math_function::log: {
      return string_from_standard_library_function(function);
    }
    case std_math_function::acos: {
      return "arccos";
    }
    case std_math_function::asin: {
      return "arcsin";
    }
    case std_math_function::atan: {
      return "arctan";
    }
    case std_math_function::acosh: {
      return "arccosh";
    }
    case std_math_function::asinh: {
      return "arcsinh";
    }
    case std_math_function::atanh: {
      return "arctanh";
    }
    case std_math_function::signum: {
      return "sign";
    }
    case std_math_function::atan2: {
      return "arctan2";
    }
    case std_math_function::powi:
    case std_math_function::powf: {
      return "power";
    }
  }
  WF_ASSERT_ALWAYS("Unhandled standard math function: {}",
                   string_from_standard_library_function(function));
}

std::string python_code_generator::operator()(const ast::call_std_function& x) const {
  return fmt::format("{}.{}({})", python_module_prefix_from_target(target_),
                     python_string_from_std_math_function(x.function),
                     fmt::join(x.args | std::views::transform(*this), ", "));
}

std::string python_code_generator::operator()(const ast::cast& x) const {
  // We use asarray because:
  // - For things that already satisfy the type, it will be a shallow copy.
  // - For other things (like numeric literals), it will perform the necessary coercion.
  return fmt::format("{}.asarray({}, dtype={}.{})", python_module_prefix_from_target(target_),
                     make_view(x.arg), python_module_prefix_from_target(target_),
                     python_string_from_cast_destination_type(x.destination_type, float_width_));
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
  return fmt::format("{}([{}]).reshape({}, {})", python_matrix_constructor_from_target(target_),
                     fmt::join(x.args | std::views::transform(*this), ", "), x.type.rows(),
                     x.type.cols());
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
  return fmt::format("{}", x.value);
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
  return std::to_string(x.value);
}

std::string python_code_generator::operator()(const ast::multiply& x) const {
  return join(" * ", x.args, *this);
}

std::string python_code_generator::operator()(const ast::negate& x) const {
  return fmt::format("-{}", make_view(x.arg));
}

std::string python_code_generator::operator()(const ast::optional_output_branch& x) const {
  std::string result = fmt::format("if compute_{}:", x.arg.name());
  join_and_indent(result, indent_, "\n", "\n", "\n", x.statements, *this);
  fmt::format_to(std::back_inserter(result), "else:\n{:{}}{} = None", "", indent_, x.arg.name());
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
  return fmt::format("{}.{}", python_module_prefix_from_target(target_),
                     x.value == symbolic_constant_enum::euler ? "e" : "pi");
}

std::string python_code_generator::operator()(const ast::ternary& x) const {
  return fmt::format("{}.where({}, {}, {})", python_module_prefix_from_target(target_),
                     make_view(x.condition), make_view(x.left), make_view(x.right));
}

std::string python_code_generator::operator()(const ast::variable_ref& x) const { return x.name; }

std::string python_code_generator::operator()(const ast::ast_element& element) const {
  return ast::visit(element, *this);
}

}  // namespace wf
