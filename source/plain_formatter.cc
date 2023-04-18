#include "plain_formatter.h"

#include <algorithm>
#include <optional>

#include <fmt/format.h>

#include "assertions.h"
#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "string_utils.h"

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
        FormatPrecedence(Precedence::Addition, multiplicand);
      } else {
        VisitStruct(-coeff, *this);
        if (!IsOne(multiplicand)) {
          output_ += " * ";
          FormatPrecedence(Precedence::Multiplication, multiplicand);
        }
      }
    } else {
      if (i > 0) {
        output_ += " + ";
      }
      if (IsOne(coeff)) {
        FormatPrecedence(Precedence::Addition, multiplicand);
      } else {
        VisitStruct(coeff, *this);
        if (!IsOne(multiplicand)) {
          output_ += " * ";
          FormatPrecedence(Precedence::Multiplication, multiplicand);
        }
      }
    }
  }
}

void PlainFormatter::Apply(const Conditional& conditional) {
  output_ += "piecewise(";
  VisitStruct(conditional.Condition(), *this);
  output_ += ", ";
  VisitStruct(conditional.IfBranch(), *this);
  output_ += ", ";
  VisitStruct(conditional.ElseBranch(), *this);
  output_ += ")";
}

void PlainFormatter::Apply(const Constant& expr) {
  output_ += StringFromSymbolicConstant(expr.GetName());
}

void PlainFormatter::Apply(const Infinity&) {
  fmt::format_to(std::back_inserter(output_), "z-inf");
}

void PlainFormatter::Apply(const Integer& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Float& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const FunctionArgument& func_arg) {
  fmt::format_to(std::back_inserter(output_), "$arg({}, {})", func_arg.ArgIndex(),
                 func_arg.ElementIndex());
}

void PlainFormatter::Apply(const Matrix& mat) {
  ASSERT_GREATER_OR_EQ(mat.NumRows(), 0);
  ASSERT_GREATER_OR_EQ(mat.NumCols(), 0);

  if (mat.Size() == 0) {
    // Empty matrix:
    output_ += "[]";
    return;
  }

  // Buffer of all the formatted elements:
  std::vector<std::string> elements;
  elements.resize(mat.Size());

  // Format all the child elements up front. That way we can do alignment:
  std::transform(mat.begin(), mat.end(), elements.begin(), [this](const Expr& expr) {
    PlainFormatter child_formatter{power_style_};
    VisitStruct(expr, child_formatter);
    return child_formatter.output_;
  });

  // Determine widest element in each column
  std::vector<std::size_t> column_widths(mat.NumCols(), 0);
  for (index_t j = 0; j < mat.NumCols(); ++j) {
    for (index_t i = 0; i < mat.NumRows(); ++i) {
      column_widths[static_cast<std::size_t>(j)] =
          std::max(column_widths[static_cast<std::size_t>(j)], elements[mat.Index(i, j)].size());
    }
  }

  output_ += "[";
  for (index_t i = 0; i < mat.NumRows(); ++i) {
    output_ += "[";
    const index_t last_col = mat.NumCols() - 1;
    for (index_t j = 0; j < last_col; ++j) {
      fmt::format_to(std::back_inserter(output_), "{:>{}}, ", elements[mat.Index(i, j)],
                     column_widths[static_cast<std::size_t>(j)]);
    }
    fmt::format_to(std::back_inserter(output_), "{:>{}}", elements[mat.Index(i, last_col)],
                   column_widths[static_cast<std::size_t>(last_col)]);
    // Insert a comma and new-line if another row is coming.
    if (i + 1 < mat.NumRows()) {
      output_ += "],\n ";
    } else {
      output_ += "]";
    }
  }
  output_ += "]";
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
  VisitStruct(func.Arg(), *this);
  output_ += ")";
}

void PlainFormatter::Apply(const Power& expr) { FormatPower(expr.Base(), expr.Exponent()); }

void PlainFormatter::Apply(const Rational& expr) {
  fmt::format_to(std::back_inserter(output_), "{} / {}", expr.Numerator(), expr.Denominator());
}

void PlainFormatter::Apply(const Relational& expr) {
  FormatPrecedence(Precedence::Relational, expr.Left());
  fmt::format_to(std::back_inserter(output_), " {} ", expr.OperationString());
  FormatPrecedence(Precedence::Relational, expr.Right());
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

void PlainFormatter::FormatPrecedence(const Precedence parent, const Expr& expr) {
  if (GetPrecedence(expr) <= parent) {
    output_ += "(";
    VisitStruct(expr, *this);
    output_ += ")";
  } else {
    VisitStruct(expr, *this);
  }
}

void PlainFormatter::FormatPower(const Expr& base, const Expr& exponent) {
  FormatPrecedence(Precedence::Power, base);
  if (power_style_ == PowerStyle::Hat) {
    output_ += " ^ ";
  } else if (power_style_ == PowerStyle::Python) {
    output_ += " ** ";
  }
  FormatPrecedence(Precedence::Power, exponent);
}

}  // namespace math
