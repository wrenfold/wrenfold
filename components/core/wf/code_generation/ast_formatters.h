#pragma once
#include "wf/code_generation/ast.h"

namespace math::ast {

//
// Formatters for the ast types.
//

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::ScalarType& s) {
  return fmt::format_to(it, "ScalarType<{}>", string_from_numeric_type(s.numeric_type()));
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::MatrixType& m) {
  return fmt::format_to(it, "MatrixType<{}, {}>", m.rows(), m.cols());
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Type& t) {
  return std::visit([it = std::move(it)](const auto& x) { return format_ast(it, x); }, t);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::VariableRef& v) {
  return fmt::format_to(it, "{}", v.name);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Add& v) {
  return fmt::format_to(it, "Add({}, {})", *v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::AssignOutputArgument& v) {
  return fmt::format_to(it, "AssignOutputArgument({} = {})", v.argument->name(),
                        fmt::join(v.values, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::AssignTemporary& v) {
  return fmt::format_to(it, "AssignTemporary({} = {})", v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Branch& d) {
  return fmt::format_to(it, "Branch(if {} {{ {} statements }} else {{ {} statements }})",
                        *d.condition, d.if_branch.size(), d.else_branch.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Call& c) {
  return fmt::format_to(it, "Call({}, {})", string_from_standard_library_function(c.function),
                        fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Cast& c) {
  return fmt::format_to(it, "Cast({}, {})", string_from_numeric_type(c.destination_type), *c.arg);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Compare& c) {
  return fmt::format_to(it, "Compare({} {} {})", *c.left,
                        string_from_relational_operation(c.operation), *c.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::ConstructReturnValue& c) {
  return fmt::format_to(it, "ConstructReturnValue({}, {})", c.type, fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Declaration& d) {
  if (d.value) {
    return fmt::format_to(it, "Declaration({}: {} = {})", d.name, string_from_numeric_type(d.type),
                          *d.value);
  } else {
    return fmt::format_to(it, "Declaration({}: {})", d.name, string_from_numeric_type(d.type));
  }
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Divide& d) {
  return fmt::format_to(it, "Divide({}, {})", *d.left, *d.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::FloatConstant& c) {
  return fmt::format_to(it, "{}f", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::InputValue& v) {
  return fmt::format_to(it, "{}[{}]", v.argument->name(), v.element);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::IntegerConstant& c) {
  return fmt::format_to(it, "{}", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::Multiply& m) {
  return fmt::format_to(it, "Multiply({}, {})", *m.left, *m.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::OptionalOutputBranch& m) {
  return fmt::format_to(it, "OptionalOutputBranch({}, <{} statements>)", m.argument->name(),
                        m.statements.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const math::ast::SpecialConstant& c) {
  return fmt::format_to(it, "SpecialConstant({})", string_from_symbolic_constant(c.value));
}

}  // namespace math::ast

// Support fmt printing of types convertible to `ast::Variant`
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<math::ast::Variant, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<math::ast::Variant, Arg>) {
      return std::visit([&](const auto& x) { return math::ast::format_ast(ctx.out(), x); }, m);
    } else {
      return format_ast(ctx.out(), m);
    }
  }
};

// Support fmt printing of types convertible to `ast::Type`
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<math::ast::Type, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<math::ast::Type, Arg>) {
      return std::visit([&](const auto& x) { return math::ast::format_ast(ctx.out(), x); }, m);
    } else {
      return format_ast(ctx.out(), m);
    }
  }
};