#include "expression.h"

#include "assertions.h"
#include "constants.h"
#include "expressions/addition.h"
#include "expressions/multiplication.h"
#include "expressions/numeric_expressions.h"
#include "expressions/power.h"
#include "expressions/relational.h"
#include "expressions/variable.h"
#include "plain_formatter.h"

namespace math {

Expr::Expr(const std::string_view name) : Expr(MakeExpr<Variable>(std::string{name})) {}

Expr Expr::FromFloat(const double x) {
  if (x == 0) {
    return Constants::Zero;
  }
  return Float::Create(x);
}

Expr Expr::FromInt(const std::int64_t x) { return Integer::Create(x); }

std::string Expr::ToString() const {
  ASSERT(impl_);
  PlainFormatter formatter{};
  Visit(*this, formatter);
  return formatter.GetOutput();
}

Expr Expr::operator-() const {
  return Multiplication::FromOperands({Constants::NegativeOne, *this});
}

Expr operator+(const Expr& a, const Expr& b) {
  // See note on absl::Span() constructor, the lifetimes here are valid.
  // We are constructing an initializer_list.
  return Addition::FromOperands({a, b});
}

Expr operator-(const Expr& a, const Expr& b) {
  return a + Multiplication::FromOperands({Constants::NegativeOne, b});
}

Expr operator*(const Expr& a, const Expr& b) { return Multiplication::FromOperands({a, b}); }

Expr operator/(const Expr& a, const Expr& b) {
  if (b.Is<Matrix>()) {
    throw TypeError("Cannot divide by a matrix expression of type: {}", b.TypeName());
  }
  auto one_over_b = Power::Create(b, Constants::NegativeOne);
  return Multiplication::FromOperands({a, one_over_b});
}

Expr operator<(const Expr& a, const Expr& b) {
  return Relational::Create(RelationalOperation::LessThan, a, b);
}

Expr operator>(const Expr& a, const Expr& b) {
  return Relational::Create(RelationalOperation::LessThan, b, a);
}

Expr operator<=(const Expr& a, const Expr& b) {
  return Relational::Create(RelationalOperation::LessThanOrEqual, a, b);
}

Expr operator>=(const Expr& a, const Expr& b) {
  return Relational::Create(RelationalOperation::LessThanOrEqual, b, a);
}

Expr operator==(const Expr& a, const Expr& b) {
  return Relational::Create(RelationalOperation::Equal, a, b);
}

// Visitor to determine mathematical precedence.
struct PrecedenceVisitor {
  using ReturnType = Precedence;

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

Precedence GetPrecedence(const Expr& expr) { return Visit(expr, PrecedenceVisitor{}); }

}  // namespace math
