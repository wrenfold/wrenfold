#pragma once
#include "wf/code_generation/ast.h"

// Formatters for the ast types. These are defined primarily so that we can implement repr() in
// python + generate prettier assertions when throwing from C++.
namespace wf::ast {

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
auto format_ast(Iterator it, const wf::ast::comment& c) {
  return fmt::format_to(it, "Comment({} characters)", c.content.size());
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
auto format_ast(Iterator it, const wf::ast::integer_literal& c) {
  return fmt::format_to(it, "{}", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::multiply& m) {
  return fmt::format_to(it, "Multiply({}, {})", *m.left, *m.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::negate& n) {
  return fmt::format_to(it, "Negate({})", *n.arg);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::optional_output_branch& m) {
  return fmt::format_to(it, "OptionalOutputBranch({}, <{} statements>)", m.arg->name(),
                        m.statements.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::read_input_scalar& r) {
  return fmt::format_to(it, "ReadInputScalar({})", r.arg->name());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::read_input_matrix& r) {
  return fmt::format_to(it, "ReadInputMatrix({}[{}, {}])", r.arg->name(), r.row, r.col);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::read_input_struct& r) {
  if (r.access_sequence.empty()) {
    return fmt::format_to(it, "ReadInputStruct({})", r.arg->name());
  }
  return fmt::format_to(it, "ReadInputStruct({}.{})", r.arg->name(),
                        fmt::join(r.access_sequence, "."));
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

// Support fmt printing of types convertible to `type_variant`
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<wf::type_variant, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<wf::type_variant, Arg>) {
      return std::visit([&](const auto& x) { return fmt::format_to(ctx.out(), "{}", x); }, m);
    } else if constexpr (std::is_same_v<wf::scalar_type, Arg>) {
      return fmt::format_to(ctx.out(), "ScalarType<{}>",
                            string_from_code_numeric_type(m.numeric_type()));
    } else if constexpr (std::is_same_v<wf::matrix_type, Arg>) {
      return fmt::format_to(ctx.out(), "MatrixType<{}, {}>", m.rows(), m.cols());
    } else {  // is_same_v<wf::custom_type::const_shared_ptr, Arg>
      return fmt::format_to(ctx.out(), "CustomType('{}')", m->name());
    }
  }
};

// Support fmt printing of types convertible to `access_variant`
template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<wf::access_variant, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<wf::access_variant, Arg>) {
      return std::visit([&](const auto& x) { return fmt::format_to(ctx.out(), "{}", x); }, m);
    } else if constexpr (std::is_same_v<wf::matrix_access, Arg>) {
      const auto [row, col] = m.indices();
      return fmt::format_to(ctx.out(), "[{}, {}]", row, col);
    } else {  // is_same_v<wf::field_access, Arg>
      return fmt::format_to(ctx.out(), "{}", m.field_name());
    }
  }
};
