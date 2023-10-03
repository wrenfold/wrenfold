#include "plain_formatter.h"

#include <algorithm>
#include <optional>

#include "assertions.h"
#include "common_visitors.h"
#include "expressions/all_expressions.h"
#include "fmt_imports.h"

namespace math {

void PlainFormatter::operator()(const Addition& expr) {
  ASSERT_GREATER_OR_EQ(expr.arity(), 2);

  // Sort into canonical order:
  absl::InlinedVector<std::pair<Expr, Expr>, 16> terms;
  terms.reserve(expr.arity());
  std::transform(expr.begin(), expr.end(), std::back_inserter(terms),
                 [](const Expr& x) { return as_coeff_and_mul(x); });

  std::sort(terms.begin(), terms.end(), [](const auto& a, const auto& b) {
    return expression_order(a.second, b.second) == RelativeOrder::LessThan;
  });

  for (std::size_t i = 0; i < terms.size(); ++i) {
    const auto [coeff, multiplicand] = terms[i];
    if (is_negative_number(coeff)) {
      if (i == 0) {
        // For the first term, just negate it:
        output_ += "-";
      } else {
        // Subsequent terms are written as subtractions:
        output_ += " - ";
      }
      // Don't multiply by negative one:
      if (is_negative_one(coeff)) {
        format_precedence(Precedence::Addition, multiplicand);
      } else {
        visit(-coeff, *this);
        if (!is_one(multiplicand)) {
          output_ += " * ";
          format_precedence(Precedence::Multiplication, multiplicand);
        }
      }
    } else {
      if (i > 0) {
        output_ += " + ";
      }
      if (is_one(coeff)) {
        format_precedence(Precedence::Addition, multiplicand);
      } else {
        visit(coeff, *this);
        if (!is_one(multiplicand)) {
          output_ += " * ";
          format_precedence(Precedence::Multiplication, multiplicand);
        }
      }
    }
  }
}

void PlainFormatter::operator()(const Conditional& conditional) {
  output_ += "where(";
  visit(conditional.condition(), *this);
  output_ += ", ";
  visit(conditional.if_branch(), *this);
  output_ += ", ";
  visit(conditional.else_branch(), *this);
  output_ += ")";
}

void PlainFormatter::operator()(const Constant& expr) {
  output_ += string_from_symbolic_constant(expr.name());
}

void PlainFormatter::operator()(const Derivative& derivative) {
  output_ += "Derivative(";
  visit(derivative.differentiand(), *this);
  output_ += ", ";
  visit(derivative.argument(), *this);
  if (derivative.order() > 1) {
    fmt::format_to(std::back_inserter(output_), ", {})", derivative.order());
  } else {
    output_ += ")";
  }
}

void PlainFormatter::operator()(const Infinity&) {
  fmt::format_to(std::back_inserter(output_), "z-inf");
}

void PlainFormatter::operator()(const Integer& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.get_value());
}

void PlainFormatter::operator()(const Float& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.get_value());
}

void PlainFormatter::operator()(const FunctionArgument& func_arg) {
  fmt::format_to(std::back_inserter(output_), "$arg({}, {})", func_arg.arg_index(),
                 func_arg.element_index());
}

void PlainFormatter::operator()(const Matrix& mat) {
  ASSERT_GREATER_OR_EQ(mat.rows(), 0);
  ASSERT_GREATER_OR_EQ(mat.cols(), 0);

  if (mat.size() == 0) {
    // Empty matrix:
    output_ += "[]";
    return;
  }

  // Buffer of all the formatted elements:
  std::vector<std::string> elements;
  elements.resize(mat.size());

  // Format all the child elements up front. That way we can do alignment:
  std::transform(mat.begin(), mat.end(), elements.begin(), [](const Expr& expr) {
    PlainFormatter child_formatter{};
    visit(expr, child_formatter);
    return child_formatter.output_;
  });

  // Determine widest element in each column
  std::vector<std::size_t> column_widths(mat.cols(), 0);
  for (index_t j = 0; j < mat.cols(); ++j) {
    for (index_t i = 0; i < mat.rows(); ++i) {
      column_widths[static_cast<std::size_t>(j)] = std::max(
          column_widths[static_cast<std::size_t>(j)], elements[mat.compute_index(i, j)].size());
    }
  }

  output_ += "[";
  for (index_t i = 0; i < mat.rows(); ++i) {
    output_ += "[";
    const index_t last_col = mat.cols() - 1;
    for (index_t j = 0; j < last_col; ++j) {
      fmt::format_to(std::back_inserter(output_), "{:>{}}, ", elements[mat.compute_index(i, j)],
                     column_widths[static_cast<std::size_t>(j)]);
    }
    fmt::format_to(std::back_inserter(output_), "{:>{}}", elements[mat.compute_index(i, last_col)],
                   column_widths[static_cast<std::size_t>(last_col)]);
    // Insert a comma and new-line if another row is coming.
    if (i + 1 < mat.rows()) {
      output_ += "],\n ";
    } else {
      output_ += "]";
    }
  }
  output_ += "]";
}

void PlainFormatter::operator()(const Multiplication& expr) {
  ASSERT_GREATER_OR_EQ(expr.arity(), 2);
  using BaseExp = MultiplicationFormattingInfo::BaseExp;

  // Break multiplication up into numerator and denominator:
  const MultiplicationFormattingInfo info = get_formatting_info(expr);

  if (info.is_negative) {
    output_ += "-";
  }

  const auto format_element = [this](const std::variant<Integer, Float, BaseExp>& element) {
    if (std::holds_alternative<Integer>(element)) {
      this->operator()(std::get<Integer>(element));
    } else if (std::holds_alternative<Float>(element)) {
      this->operator()(std::get<Float>(element));
    } else {
      const BaseExp& pow = std::get<BaseExp>(element);
      if (is_one(pow.exponent)) {
        this->format_precedence(Precedence::Multiplication, pow.base);
      } else {
        this->format_power(pow.base, pow.exponent);
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

void PlainFormatter::operator()(const Function& func) {
  fmt::format_to(std::back_inserter(output_), "{}(", func.function_name());
  auto it = func.begin();
  if (it != func.end()) {
    visit(*it, *this);
  }
  for (++it; it != func.end(); ++it) {
    output_ += ", ";
    visit(*it, *this);
  }
  output_ += ")";
}

void PlainFormatter::operator()(const Power& expr) { format_power(expr.base(), expr.exponent()); }

void PlainFormatter::operator()(const Rational& expr) {
  fmt::format_to(std::back_inserter(output_), "{} / {}", expr.numerator(), expr.denominator());
}

void PlainFormatter::operator()(const Relational& expr) {
  format_precedence(Precedence::Relational, expr.left());
  fmt::format_to(std::back_inserter(output_), " {} ", expr.operation_string());
  format_precedence(Precedence::Relational, expr.right());
}

void PlainFormatter::operator()(const Variable& expr) { output_ += expr.name(); }

void PlainFormatter::format_precedence(const Precedence parent, const Expr& expr) {
  if (get_precedence(expr) <= parent) {
    output_ += "(";
    visit(expr, *this);
    output_ += ")";
  } else {
    visit(expr, *this);
  }
}

void PlainFormatter::format_power(const Expr& base, const Expr& exponent) {
  format_precedence(Precedence::Power, base);
  output_ += " ** ";
  format_precedence(Precedence::Power, exponent);
}

}  // namespace math
