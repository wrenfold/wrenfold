#include "plain_formatter.h"

#include <optional>
#include <type_traits>
#include <vector>

#include <fmt/format.h>

#include "assertions.h"
#include "common_visitors.h"
#include "expressions/all_expressions.h"

namespace math {

void PlainFormatter::Apply(const Addition& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);
  // Format the first arg:
  expr[0].Receive(*this);
  for (std::size_t i = 1; i < expr.Arity(); ++i) {
    //

    //    const std::optional<const Expr*> negation_inner =
    //        VisitLambda(expr[i], [](const Negation& n) { return &n.Inner(); });
    //    if (false) {  // todo: fix
    //      // Format subtractions in a pretty way.
    //      output_ += " - ";
    //      (*negation_inner)->Receive(*this);
    //    } else {
    output_ += " + ";
    expr[i].Receive(*this);
    //    }
  }
}

void PlainFormatter::Apply(const Constant& expr) {
  output_ += StringFromSymbolicConstant(expr.GetName());
}

void PlainFormatter::Apply(const Integer& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Float& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

struct FormatNumberVisitor {
  using ReturnType = std::string;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;
  ReturnType Apply(const Integer& i) const { return std::to_string(i.GetValue()); }
  ReturnType Apply(const Float& f) const { return std::to_string(f.GetValue()); }
};

void PlainFormatter::Apply(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);

  const auto [numerator, denominator] = expr.SplitByExponent();

  // Be careful about recursion here:
  if (const Multiplication* const mul = TryCast<Multiplication>(numerator); mul != nullptr) {
    FormatMultiplication(*mul);
  } else {
    FormatPrecedence(Precedence::Multiplication, numerator);
  }

  if (IsOne(denominator)) {
    return;
  }

  output_ += " / ";

  if (const Multiplication* const mul = TryCast<Multiplication>(denominator); mul != nullptr) {
    output_ += "(";
    FormatMultiplication(*mul);
    output_ += ")";
  } else {
    FormatPrecedence(Precedence::Multiplication, denominator);
  }
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Power& expr) { FormatPower(expr.Base(), expr.Exponent()); }

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

void PlainFormatter::VisitWithBrackets(const Expr& expr) {
  output_ += "(";
  expr.Receive(*this);
  output_ += ")";
}

void PlainFormatter::FormatPrecedence(const Precedence parent, const Expr& expr) {
  if (GetPrecedence(expr) <= parent) {
    VisitWithBrackets(expr);
  } else {
    expr.Receive(*this);
  }
}

void PlainFormatter::FormatPower(const Expr& Base, const Expr& Exponent) {
  FormatPrecedence(Precedence::Power, Base);
  output_ += " ^ ";
  FormatPrecedence(Precedence::Power, Exponent);
}

void PlainFormatter::FormatMultiplication(const Multiplication& mul) {
  const Expr& constant_term = mul[0];
  if (FormatMultiplicationConstant(constant_term)) {
    output_ += " * ";
  }
  FormatPrecedence(Precedence::Multiplication, mul[1]);
  for (std::size_t i = 2; i < mul.Arity(); ++i) {
    output_ += " * ";
    FormatPrecedence(Precedence::Multiplication, mul[i]);
  }
}

bool PlainFormatter::FormatMultiplicationConstant(const Expr& c) {
  if (IsOne(c)) {
    return false;
  } else if (c.IsIdenticalTo(Constants::NegativeOne)) {
    output_ += "-";
    return false;
  }
  c.Receive(*this);
  return true;
}

}  // namespace math
