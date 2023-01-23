#include "plain_formatter.h"

#include <optional>

#include <fmt/format.h>

#include "assertions.h"
#include "common_visitors.h"
#include "expressions/all_expressions.h"

namespace math {

void PlainFormatter::Apply(const Addition& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);
  for (std::size_t i = 0; i < expr.Arity(); ++i) {
    const auto [coeff, multiplicand] = AsCoefficientAndMultiplicand(expr[i]);
    if (IsNegativeNumber(coeff)) {
      if (i == 0) {
        // For the first term, just negate it:
        output_ += "-";
      } else {
        // Subsequent terms are written as subtractions:
        output_ += " - ";
      }
      // Don't multiply by negative one:
      if (IsNegativeOne(coeff)) {
        multiplicand.Receive(*this);
      } else {
        coeff.Receive(*this);
        if (!IsOne(multiplicand)) {
          output_ += " * ";
          multiplicand.Receive(*this);
        }
      }
    } else {
      if (i > 0) {
        output_ += " + ";
      }
      if (IsOne(coeff)) {
        multiplicand.Receive(*this);
      } else {
        coeff.Receive(*this);
        if (!IsOne(multiplicand)) {
          output_ += " * ";
          multiplicand.Receive(*this);
        }
      }
    }
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

void PlainFormatter::Apply(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.Arity(), 2);
  using BaseExp = MultiplicationFormattingInfo::BaseExp;

  // Break multiplication up into numerator and denominator:
  const MultiplicationFormattingInfo info = GetFormattingInfo(expr);

  if (info.is_negative) {
    output_ += "-";
  }

  const auto format_element = [this](const std::variant<Integer, Float, BaseExp>& element) {
    if (std::holds_alternative<Integer>(element)) {
      this->Apply(std::get<Integer>(element));
    } else if (std::holds_alternative<Float>(element)) {
      this->Apply(std::get<Float>(element));
    } else {
      const BaseExp& pow = std::get<BaseExp>(element);
      if (IsOne(pow.exponent)) {
        this->FormatPrecedence(Precedence::Multiplication, pow.base);
      } else {
        this->FormatPower(pow.base, pow.exponent);
      }
    }
  };

  format_element(info.numerator.front());
  for (std::size_t i = 1; i < info.numerator.size(); ++i) {
    output_ += " * ";
    format_element(info.numerator[i]);
  }

  if (info.denominator.empty()) {
    return;
  }
  output_ += " / ";

  if (info.denominator.size() > 1) {
    output_ += "(";
    format_element(info.denominator.front());
    for (std::size_t i = 1; i < info.denominator.size(); ++i) {
      output_ += " * ";
      format_element(info.denominator[i]);
    }
    output_ += ")";
  } else {
    format_element(info.denominator.front());
  }
}

void PlainFormatter::Apply(const UnaryFunction& func) {
  fmt::format_to(std::back_inserter(output_), "{}(", func.Name());
  func.Arg().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Power& expr) { FormatPower(expr.Base(), expr.Exponent()); }

void PlainFormatter::Apply(const Rational& expr) {
  fmt::format_to(std::back_inserter(output_), "{} / {}", expr.Numerator(), expr.Denominator());
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

void PlainFormatter::FormatPrecedence(const Precedence parent, const Expr& expr) {
  if (GetPrecedence(expr) <= parent) {
    output_ += "(";
    expr.Receive(*this);
    output_ += ")";
  } else {
    expr.Receive(*this);
  }
}

void PlainFormatter::FormatPower(const Expr& base, const Expr& exponent) {
  FormatPrecedence(Precedence::Power, base);
  output_ += " ^ ";
  FormatPrecedence(Precedence::Power, exponent);
}

}  // namespace math
