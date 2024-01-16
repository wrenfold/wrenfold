#pragma once
#include "wf/code_generation/ast.h"
#include "wf/code_generation/string_util.h"

// Formatters for the ast types. These are defined primarily so that we can implement repr() in
// python + generate prettier assertions when throwing from C++.
namespace wf::ast {

// Return null terminated string with camel case name for one of the AST types.
// Returns const-char so it can be passed to pybind11.
template <typename T>
const char* camel_case_name() noexcept {
  // We can't make this array static constexpr w/ gcc (but it works on msvc).
  constexpr std::array array = camel_case_from_snake_case<64>(T::snake_case_name_str);
  static const std::string str{array.data()};
  return str.c_str();
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::scalar_type& s) {
  return fmt::format_to(it, "<{}>", string_from_code_numeric_type(s.numeric_type()));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::matrix_type& m) {
  return fmt::format_to(it, "<{}, {}>", m.rows(), m.cols());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::custom_type& c) {
  return fmt::format_to(it, "('{}')", c.name());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::variable_ref& v) {
  return fmt::format_to(it, "({})", v.name);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::add& v) {
  return fmt::format_to(it, "({}, {})", *v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_output_matrix& v) {
  return fmt::format_to(it, "({} = <{} values>)", v.arg.name(), v.value->args.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_output_scalar& v) {
  return fmt::format_to(it, "({} = {})", v.arg.name(), *v.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_output_struct& v) {
  return fmt::format_to(it, "({} = {})", v.arg.name(), *v.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::assign_temporary& v) {
  return fmt::format_to(it, "({} = {})", v.left, *v.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::branch& d) {
  return fmt::format_to(it, "(if {} {{ {} statements }} else {{ {} statements }})", *d.condition,
                        d.if_branch.size(), d.else_branch.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::call_custom_function& c) {
  return fmt::format_to(it, "({}, {})", c.function.name(), fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::call_std_function& c) {
  return fmt::format_to(it, "({}, {})", string_from_standard_library_function(c.function),
                        fmt::join(c.args, ", "));
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::cast& c) {
  return fmt::format_to(it, "({}, {})", string_from_code_numeric_type(c.destination_type), *c.arg);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::comment& c) {
  return fmt::format_to(it, "({} characters)", c.content.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::compare& c) {
  return fmt::format_to(it, "({} {} {})", *c.left, string_from_relational_operation(c.operation),
                        *c.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::construct_matrix& c) {
  if (c.args.size() <= 4) {
    return fmt::format_to(it, "({}, {})", c.type, fmt::join(c.args, ", "));
  } else {
    return fmt::format_to(it, "({}, <{} values>)", c.type, c.args.size());
  }
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::construct_custom_type& c) {
  return fmt::format_to(it, "({}, {} fields)", c.type, c.field_values.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::declaration& d) {
  if (d.value) {
    return fmt::format_to(it, "({}: {} = {})", d.name, string_from_code_numeric_type(d.type),
                          *d.value);
  } else {
    return fmt::format_to(it, "({}: {})", d.name, string_from_code_numeric_type(d.type));
  }
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::divide& d) {
  return fmt::format_to(it, "({}, {})", *d.left, *d.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::float_literal& c) {
  return fmt::format_to(it, "({}f)", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::function_definition& f) {
  return fmt::format_to(it, "('{}', <{} arguments>, <{} elements>)", f.signature().name(),
                        f.signature().num_arguments(), f.body().size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::function_signature& f) {
  return fmt::format_to(it, "('{}', <{} arguments>)", f.name(), f.num_arguments());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::get_argument& r) {
  return fmt::format_to(it, "({})", r.arg.name());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::get_field& r) {
  return fmt::format_to(it, "({}, {})", *r.arg, r.field);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::get_matrix_element& r) {
  return fmt::format_to(it, "({}, [{}, {}])", *r.arg, r.row, r.col);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::integer_literal& c) {
  return fmt::format_to(it, "({})", c.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::multiply& m) {
  return fmt::format_to(it, "({}, {})", *m.left, *m.right);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::negate& n) {
  return fmt::format_to(it, "({})", *n.arg);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::optional_output_branch& m) {
  return fmt::format_to(it, "({}, <{} statements>)", m.arg.name(), m.statements.size());
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::return_type_annotation& r) {
  if (r.type) {
    return fmt::format_to(it, "({})", *r.type);
  } else {
    return fmt::format_to(it, "(None)");
  }
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::return_object& r) {
  return fmt::format_to(it, "({})", *r.value);
}

template <typename Iterator>
auto format_ast(Iterator it, const wf::ast::special_constant& c) {
  return fmt::format_to(it, "({})", string_from_symbolic_constant(c.value));
}

// True if `format_ast` is implemented for the the type `T`.
template <typename T, typename = void>
struct is_formattable : std::false_type {};
template <typename T>
struct is_formattable<T, decltype(format_ast(std::declval<std::back_insert_iterator<std::string>>(),
                                             std::declval<const T>()),
                                  void())> : std::true_type {};
template <typename T, typename Type>
using enable_if_is_formattable_t = std::enable_if_t<is_formattable<T>::value, Type>;

}  // namespace wf::ast

// Support fmt printing of types that implement format_to.
template <typename T>
struct fmt::formatter<T, wf::ast::enable_if_is_formattable_t<T, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    auto it = fmt::format_to(ctx.out(), "{}", wf::ast::camel_case_name<Arg>());
    return wf::ast::format_ast(it, m);
  }
};

template <>
struct fmt::formatter<wf::ast::variant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& v, FormatContext& ctx) const -> decltype(ctx.out()) {
    return std::visit([&](const auto& x) { return fmt::format_to(ctx.out(), "{}", x); }, v);
  }
};

template <>
struct fmt::formatter<wf::type_variant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& v, FormatContext& ctx) const -> decltype(ctx.out()) {
    return std::visit([&](const auto& x) { return fmt::format_to(ctx.out(), "{}", x); }, v);
  }
};
