#include "expression.h"

#include "assertions.h"
#include "constants.h"
#include "expressions/all_expressions.h"
#include "plain_formatter.h"

namespace math {

Expr::Expr(const std::string_view name) : Expr(make_expr<Variable>(std::string{name})) {}

Expr Expr::from_float(const double x) {
  if (x == 0) {
    return Constants::Zero;
  }
  return Float::create(x);
}

Expr Expr::from_int(const std::int64_t x) { return Integer::create(x); }

std::string Expr::to_string() const {
  PlainFormatter formatter{};
  visit(*this, formatter);
  return formatter.take_output();
}

Expr Expr::operator-() const {
  return Multiplication::from_operands({Constants::NegativeOne, *this});
}

Expr operator+(const Expr& a, const Expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return Addition::from_operands({a, b});
}

Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::from_operands({Constants::NegativeOne, b});
}

Expr operator*(const Expr& a, const Expr& b) { return Multiplication::from_operands({a, b}); }

Expr operator/(const Expr& a, const Expr& b) {
  auto one_over_b = Power::create(b, Constants::NegativeOne);
  return Multiplication::from_operands({a, one_over_b});
}

Expr operator<(const Expr& a, const Expr& b) {
  return Relational::create(RelationalOperation::LessThan, a, b);
}

Expr operator>(const Expr& a, const Expr& b) {
  return Relational::create(RelationalOperation::LessThan, b, a);
}

Expr operator<=(const Expr& a, const Expr& b) {
  return Relational::create(RelationalOperation::LessThanOrEqual, a, b);
}

Expr operator>=(const Expr& a, const Expr& b) {
  return Relational::create(RelationalOperation::LessThanOrEqual, b, a);
}

Expr operator==(const Expr& a, const Expr& b) {
  return Relational::create(RelationalOperation::Equal, a, b);
}

// Visitor to determine mathematical precedence.
struct PrecedenceVisitor {
  template <typename T>
  constexpr Precedence operator()(const T&) const {
    if constexpr (std::is_same_v<Multiplication, T>) {
      return Precedence::Multiplication;
    } else if constexpr (std::is_same_v<Addition, T>) {
      return Precedence::Addition;
    } else if constexpr (std::is_same_v<Power, T>) {
      return Precedence::Power;
    } else if constexpr (std::is_same_v<Rational, T>) {
      return Precedence::Multiplication;
    } else if constexpr (std::is_same_v<Relational, T>) {
      return Precedence::Relational;
    } else {
      return Precedence::None;
    }
  }
};

Precedence get_precedence(const Expr& expr) { return visit(expr, PrecedenceVisitor{}); }

}  // namespace math
