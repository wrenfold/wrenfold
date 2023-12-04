// Copyright 2023 Gareth Cross
#include "wf/code_generation/cpp_code_generator.h"

#include "wf/code_generation/ast_formatters.h"

namespace math {

static constexpr std::string_view UtilityNamespace = "math";

std::string cpp_code_generator::generate_code(const ast::function_signature& signature,
                                              const std::vector<ast::variant>& body) const {
  code_formatter result;
  format_signature(result, signature);
  result.with_indentation(2, "{\n", "\n}", [&] {
    // Convert input args to spans:
    std::size_t counter = 0;
    for (const auto& arg : signature.arguments) {
      if (arg->is_matrix()) {
        const ast::matrix_type& mat = std::get<ast::matrix_type>(arg->type());

        // Generate matrix conversion logic.
        // TODO: Support dynamic sizes here too.
        result.format("auto _{} = ", arg->name());
        const std::string dims_type = fmt::format("{}, {}", mat.rows(), mat.cols());
        switch (arg->direction()) {
          case ast::argument_direction::input:
            result.format("{}::make_input_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
          case ast::argument_direction::output:
            result.format("{}::make_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
          case ast::argument_direction::optional_output:
            result.format("{}::make_optional_output_span<{}>({});\n", UtilityNamespace, dims_type,
                          arg->name());
            break;
        }
        ++counter;
      }
    }

    if (counter > 0) {
      result.format("\n");
    }

    result.join(*this, "\n", body);
  });
  return result.get_output();
}

constexpr static std::string_view cpp_string_from_numeric_cast_type(
    const NumericType destination_type) noexcept {
  switch (destination_type) {
    case NumericType::Bool:
      return "bool";
    case NumericType::Integer:
      return "std::int64_t";
    case NumericType::Real:
      return "Scalar";
    case NumericType::Complex:
      return "std::complex<Scalar>";
  }
  return "<INVALID ENUM VALUE>";
}

void cpp_code_generator::format_signature(code_formatter& formatter,
                                          const ast::function_signature& signature) const {
  formatter.format("template <typename Scalar");
  const bool has_matrix_args =
      std::any_of(signature.arguments.begin(), signature.arguments.end(),
                  [](const std::shared_ptr<const ast::argument>& arg) { return arg->is_matrix(); });

  if (has_matrix_args) {
    std::size_t counter = 0;
    for (const std::shared_ptr<const ast::argument>& arg : signature.arguments) {
      if (arg->is_matrix()) {
        formatter.format(", typename T{}", counter);
        ++counter;
      }
    }
  }
  formatter.format(">\n");

  if (signature.return_value) {
    if (!std::holds_alternative<ast::scalar_type>(*signature.return_value)) {
      // TODO: To support returning matrices in C++ we need more than just a `span` type.
      throw type_error("Only scalars can be returned.");
    } else {
      formatter.format("auto {}(", signature.function_name);
    }
  } else {
    formatter.format("void {}(", signature.function_name);
  }

  std::size_t counter = 0;
  auto arg_printer = [&counter](code_formatter& formatter, const ast::argument::shared_ptr& arg) {
    if (arg->is_matrix()) {
      if (arg->direction() == ast::argument_direction::input) {
        formatter.format("const T{}&", counter);
      } else {
        formatter.format("T{}&&", counter);
      }
      ++counter;
    } else {
      const NumericType numeric_type = std::get<ast::scalar_type>(arg->type()).numeric_type();
      if (arg->direction() == ast::argument_direction::input) {
        formatter.format("const {}", cpp_string_from_numeric_cast_type(numeric_type));
      } else {
        // Output reference for now.
        formatter.format("{}&", cpp_string_from_numeric_cast_type(numeric_type));
      }
    }

    formatter.format(" {}", arg->name());
  };

  formatter.join(std::move(arg_printer), ", ", signature.arguments);
  formatter.format(")\n");
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::add& x) const {
  formatter.format("{} + {}", make_view(x.left), make_view(x.right));
}

void cpp_code_generator::operator()(code_formatter& formatter,
                                    const ast::assign_output_argument& assignment) const {
  const auto& dest_name = assignment.argument->name();
  const ast::argument_type& type = assignment.argument->type();

  if (std::holds_alternative<ast::matrix_type>(type)) {
    const ast::matrix_type mat = std::get<ast::matrix_type>(type);
    auto range = make_range<std::size_t>(0, assignment.values.size());

    // TODO: If there is a unit dimension, use the [] operator?
    formatter.join(
        [&](code_formatter& fmt, std::size_t i) {
          const auto [row, col] = mat.compute_indices(i);
          fmt.format("{}{}({}, {}) = {};", assignment.argument->is_matrix() ? "_" : "", dest_name,
                     row, col, make_view(assignment.values[i]));
        },
        "\n", range);

  } else {
    // Otherwise it is a scalar, so just assign it:
    WF_ASSERT_EQUAL(1, assignment.values.size());
    formatter.format("{} = {};", dest_name, assignment.values.front());
  }
}

void cpp_code_generator::operator()(code_formatter& formatter,
                                    const ast::assign_temporary& x) const {
  formatter.format("{} = {};", x.left, make_view(x.right));
}

static constexpr std::string_view cpp_string_for_std_function(const StdMathFunction name) noexcept {
  switch (name) {
    case StdMathFunction::Cos:
      return "std::cos";
    case StdMathFunction::Sin:
      return "std::sin";
    case StdMathFunction::Tan:
      return "std::tan";
    case StdMathFunction::ArcCos:
      return "std::acos";
    case StdMathFunction::ArcSin:
      return "std::asin";
    case StdMathFunction::ArcTan:
      return "std::atan";
    case StdMathFunction::Log:
      return "std::log";
    case StdMathFunction::Sqrt:
      return "std::sqrt";
    case StdMathFunction::Abs:
      return "std::abs";
    case StdMathFunction::Arctan2:
      return "std::atan2";
    case StdMathFunction::Powi:
    case StdMathFunction::Powf:
      return "std::pow";
    default:
      break;
  }
  return "<INVALID ENUM VALUE>";
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::call& x) const {
  if (x.function == StdMathFunction::Signum) {
    // We need to special-case signum because it doesn't exist as a free-standing function.
    // TODO: This should be an int expression.
    formatter.format(
        "static_cast<Scalar>(static_cast<Scalar>(0) < {}) - ({} < static_cast<Scalar>(0))",
        make_view(x.args[0]), make_view(x.args[0]));
  } else {
    formatter.format("{}({})", cpp_string_for_std_function(x.function),
                     make_join_view(*this, ", ", x.args));
  }
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::cast& x) const {
  formatter.format("static_cast<{}>({})", cpp_string_from_numeric_cast_type(x.destination_type),
                   make_view(x.arg));
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::compare& x) const {
  formatter.format("{} {} {}", make_view(x.left), string_from_relational_operation(x.operation),
                   make_view(x.right));
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::branch& x) const {
  formatter.format("if ({}) ", make_view(x.condition));
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.if_branch); });
  if (!x.else_branch.empty()) {
    formatter.with_indentation(2, " else {\n", "\n}",
                               [&] { formatter.join(*this, "\n", x.else_branch); });
  }
}

void cpp_code_generator::operator()(code_formatter& formatter,
                                    const ast::construct_return_value& x) const {
  WF_ASSERT(std::holds_alternative<ast::scalar_type>(x.type), "We cannot return matrices");
  WF_ASSERT_EQUAL(1, x.args.size());
  formatter.format("return {};", make_view(x.args[0]));
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::declaration& x) const {
  if (!x.value) {
    formatter.format("{} {};", cpp_string_from_numeric_cast_type(x.type), x.name);
  } else {
    formatter.format("const {} {} = {};", cpp_string_from_numeric_cast_type(x.type), x.name,
                     make_view(x.value));
  }
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::divide& x) const {
  formatter.format("{} / {}", make_view(x.left), make_view(x.right));
}

static constexpr std::string_view cpp_string_for_symbolic_constant(
    const SymbolicConstants value) noexcept {
  switch (value) {
    case SymbolicConstants::Euler:
      return "M_E";
    case SymbolicConstants::Pi:
      return "M_PI";
    case SymbolicConstants::True:
      return "true";
    case SymbolicConstants::False:
      return "false";
  }
  return "<INVALID ENUM VALUE>";
}

void cpp_code_generator::operator()(code_formatter& formatter,
                                    const ast::special_constant& x) const {
  formatter.format("static_cast<Scalar>({})", cpp_string_for_symbolic_constant(x.value));
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::input_value& x) const {
  WF_ASSERT(x.argument);
  if (std::holds_alternative<ast::scalar_type>(x.argument->type())) {
    formatter.format(x.argument->name());
  } else {
    const ast::matrix_type& mat = std::get<ast::matrix_type>(x.argument->type());
    const auto [r, c] = mat.compute_indices(x.element);
    formatter.format("_{}({}, {})", x.argument->name(), r, c);
  }
}

void cpp_code_generator::operator()(code_formatter& formatter, const ast::multiply& x) const {
  formatter.format("{} * {}", make_view(x.left), make_view(x.right));
}

void cpp_code_generator::operator()(code_formatter& formatter,
                                    const ast::optional_output_branch& x) const {
  formatter.format("if (static_cast<bool>({}{})) ", x.arg->is_matrix() ? "_" : "", x.arg->name());
  formatter.with_indentation(2, "{\n", "\n}", [&] { formatter.join(*this, "\n", x.statements); });
}

}  // namespace math
