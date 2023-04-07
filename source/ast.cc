// Copyright 2023 Gareth Cross
#include "ast.h"

template <typename Iterator>
auto Format(Iterator it, const math::ast::ScalarType&) {
  return fmt::format_to(it, "ScalarType");
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::MatrixType& m) {
  return fmt::format_to(it, "MatrixType<{}, {}>", m.NumRows(), m.NumCols());
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Type& t) {
  return std::visit([it = std::move(it)](const auto& x) { return Format(it, x); }, t);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::VariableRef& v) {
  return fmt::format_to(it, "{}", v.name);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Add& v) {
  return fmt::format_to(it, "Add({}, {})", *v.left, *v.right);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Assignment& v) {
  return std::visit(
      [&it, &v](const auto& left) {
        using T = std::decay_t<decltype(left)>;
        if constexpr (std::is_same_v<math::ast::VariableRef, T>) {
          return fmt::format_to(it, "Assignment({} = {})", left, *v.right);
        } else {
          return fmt::format_to(it, "Assignment({} = {})", left->Name(), *v.right);
        }
      },
      v.left);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Call& c) {
  return fmt::format_to(it, "Call({}, {})",
                        std::visit([](const auto& f) { return math::ToString(f); }, c.function),
                        fmt::join(c.args, ", "));
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Conditional& d) {
  return fmt::format_to(it, "Conditional(if {} {{ {} statements }} else {{ {} statements }})",
                        *d.condition, d.if_branch.size(), d.else_branch.size());
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::ConstructMatrix& c) {
  return fmt::format_to(it, "ConstructMatrix({})", c.type, fmt::join(c.args, ", "));
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Declaration& d) {
  return fmt::format_to(it, "Declaration({}: {} = {})", d.name, d.type, *d.value);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::FloatConstant& c) {
  return fmt::format_to(it, "{}f", c.value);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::InputValue& v) {
  return fmt::format_to(it, "{}[{}]", v.argument->Name(), v.element);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::IntegerConstant& c) {
  return fmt::format_to(it, "{}", c.value);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::Multiply& m) {
  return fmt::format_to(it, "Multiply({}, {})", *m.left, *m.right);
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::OutputExists& m) {
  return fmt::format_to(it, "OutputExists({})", m.argument->Name());
}

template <typename Iterator>
auto Format(Iterator it, const math::ast::ReturnValue& m) {
  return fmt::format_to(it, "ReturnValue({})", fmt::join(m.values, ", "));
}

template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<math::ast::Variant, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<math::ast::Variant, Arg>) {
      return std::visit([&](const auto& x) { return Format(ctx.out(), x); }, m);
    } else {
      return Format(ctx.out(), m);
    }
  }
};

template <typename T>
struct fmt::formatter<T, std::enable_if_t<std::is_constructible_v<math::ast::Type, T>, char>> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename Arg, typename FormatContext>
  auto format(const Arg& m, FormatContext& ctx) const -> decltype(ctx.out()) {
    if constexpr (std::is_same_v<math::ast::Type, Arg>) {
      return std::visit([&](const auto& x) { return Format(ctx.out(), x); }, m);
    } else {
      return Format(ctx.out(), m);
    }
  }
};

namespace math::ast {

template <typename T>
std::string FormatThis(const T& x) {
  std::string out;
  out.reserve(32);
  Format(std::back_inserter(out), x);
  return out;
}

std::string ScalarType::ToString() const { return FormatThis(*this); }

std::string MatrixType::ToString() const { return FormatThis(*this); }

std::string VariableRef::ToString() const { return FormatThis(*this); }

std::string Add::ToString() const { return FormatThis(*this); }

std::string Assignment::ToString() const { return FormatThis(*this); }

std::string Call::ToString() const { return FormatThis(*this); }

std::string Conditional::ToString() const { return FormatThis(*this); }

std::string ConstructMatrix::ToString() const { return FormatThis(*this); }

std::string Declaration::ToString() const { return FormatThis(*this); }

std::string FloatConstant::ToString() const { return FormatThis(*this); }

std::string InputValue::ToString() const { return FormatThis(*this); }

std::string IntegerConstant::ToString() const { return FormatThis(*this); }

std::string Multiply::ToString() const { return FormatThis(*this); }

std::string OutputExists::ToString() const { return FormatThis(*this); }

std::string ReturnValue::ToString() const { return FormatThis(*this); }

}  // namespace math::ast
