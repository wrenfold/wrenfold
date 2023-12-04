#include "wf/expression.h"

#include "wf/assertions.h"
#include "wf/constants.h"
#include "wf/expressions/all_expressions.h"
#include "wf/plain_formatter.h"

namespace math {

Expr::Expr(const std::string_view name, const number_set set)
    : Expr(make_expr<Variable>(NamedVariable(name), set)) {}

Expr Expr::from_float(const double x) {
  if (x == 0) {
    return constants::zero;
  }
  return Float::create(x);
}

Expr Expr::from_int(const std::int64_t x) { return Integer::create(x); }

std::string Expr::to_string() const {
  plain_formatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

Expr Expr::operator-() const {
  return Multiplication::from_operands({constants::negative_one, *this});
}

Expr operator+(const Expr& a, const Expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return Addition::from_operands({a, b});
}

Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::from_operands({constants::negative_one, b});
}

Expr operator*(const Expr& a, const Expr& b) { return Multiplication::from_operands({a, b}); }

Expr operator/(const Expr& a, const Expr& b) {
  auto one_over_b = Power::create(b, constants::negative_one);
  return Multiplication::from_operands({a, one_over_b});
}

Expr operator<(const Expr& a, const Expr& b) {
  return Relational::create(relational_operation::less_than, a, b);
}

Expr operator>(const Expr& a, const Expr& b) {
  return Relational::create(relational_operation::less_than, b, a);
}

Expr operator<=(const Expr& a, const Expr& b) {
  return Relational::create(relational_operation::less_than_or_equal, a, b);
}

Expr operator>=(const Expr& a, const Expr& b) {
  return Relational::create(relational_operation::less_than_or_equal, b, a);
}

Expr operator==(const Expr& a, const Expr& b) {
  return Relational::create(relational_operation::equal, a, b);
}

// Visitor to determine mathematical precedence.
struct PrecedenceVisitor {
  template <typename T>
  constexpr precedence operator()(const T&) const {
    if constexpr (std::is_same_v<Multiplication, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<Addition, T>) {
      return precedence::addition;
    } else if constexpr (std::is_same_v<Power, T>) {
      return precedence::power;
    } else if constexpr (std::is_same_v<Rational, T>) {
      return precedence::multiplication;
    } else if constexpr (std::is_same_v<Relational, T>) {
      return precedence::relational;
    } else {
      return precedence::none;
    }
  }
};

precedence get_precedence(const Expr& expr) { return visit(expr, PrecedenceVisitor{}); }

Expr make_unique_variable_symbol(number_set set) {
  return make_expr<Variable>(UniqueVariable(), set);
}

}  // namespace math
