#pragma once
#include "wf/code_generation/ast.h"

namespace wf::ast {

//
// Formatters for the ast types.
//

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::scalar_type& s) {
  return fmt::format_to(it, "ScalarType<{}>", string_from_code_numeric_type(s.numeric_type()));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::matrix_type& m) {
  return fmt::format_to(it, "MatrixType<{}, {}>", m.rows(), m.cols());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::argument_type& t) {
  return std::visit([it = std::move(it)](const auto& x) { return format_ast(it, x); }, t);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::variable_ref& v) {
  return fmt::format_to(it, "{}", v.name);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::add& v) {
  return fmt::format_to(it, "Add({}, {})", *v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_output_argument& v) {
  return fmt::format_to(it, "AssignOutputArgument({} = {})", v.arg->name(),
                        fmt::join(v.values, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_temporary& v) {
  return fmt::format_to(it, "AssignTemporary({} = {})", v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::branch& d) {
  return fmt::format_to(it, "Branch(if {} {{ {} statements }} else {{ {} statements }})",
                        *d.condition, d.if_branch.size(), d.else_branch.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::call& c) {
  return fmt::format_to(it, "Call({}, {})", string_from_standard_library_function(c.function),
                        fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::cast& c) {
  return fmt::format_to(it, "Cast({}, {})", string_from_code_numeric_type(c.destination_type),
                        *c.arg);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::compare& c) {
  return fmt::format_to(it, "Compare({} {} {})", *c.left,
                        string_from_relational_operation(c.operation), *c.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::construct_return_value& c) {
  return fmt::format_to(it, "ConstructReturnValue({}, {})", c.type, fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::declaration& d) {
  if (d.value) {
    return fmt::format_to(it, "Declaration({}: {} = {})", d.name,
                          string_from_code_numeric_type(d.type), *d.value);
  } else {
    return fmt::format_to(it, "Declaration({}: {})", d.name, string_from_code_numeric_type(d.type));
  }
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::divide& d) {
  return fmt::format_to(it, "Divide({}, {})", *d.left, *d.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::float_literal& c) {
  return fmt::format_to(it, "{}f", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::input_value& v) {
  return fmt::format_to(it, "{}[{}]", v.arg->name(), v.element);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::integer_literal& c) {
  return fmt::format_to(it, "{}", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::multiply& m) {
  return fmt::format_to(it, "Multiply({}, {})", *m.left, *m.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::optional_output_branch& m) {
  return fmt::format_to(it, "OptionalOutputBranch({}, <{} statements>)", m.arg->name(),
                        m.statements.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::special_constant& c) {
  return fmt::format_to(it, "SpecialConstant({})", string_from_symbolic_constant(c.value));
}

}  // namespace wf::ast

// Support fmt printing of types convertible to `ast::variant`
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<wf::ast::variant, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<wf::ast::variant, Arg>) {
      return std::visit([&](const auto& x) { return wf::ast::format_ast(ctx.out(), x); }, m);
    } else {
      return format_ast(ctx.out(), m);
    }
  }
};

// Support fmt printing of types convertible to `ast::Type`
template <typename T>
struct fmt::formatter<T,
                      std::enable_if_t<std::is_constructible_v<wf::ast::argument_type, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<wf::ast::argument_type, Arg>) {
      return std::visit([&](const auto& x) { return wf::ast::format_ast(ctx.out(), x); }, m);
    } else {
      return format_ast(ctx.out(), m);
    }
  }
};
