#include "formatting.h"

#include <fmt/format.h>

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

static int GetPrecedence(const ExpressionBase& expr) {
  const OperationBase* const op = expr.As<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

// Helper for appropriately applying brackets, considering operator precedence.
static void FormatBinaryOp(const Formatter& formatter, const int parent_precedence,
                           const char* const op_str, const ExpressionBase& a,
                           const ExpressionBase& b, std::string& output_str) {
  if (parent_precedence > GetPrecedence(a)) {
    output_str += "(";
    a.Format(formatter, output_str);
    output_str += ")";
  } else {
    a.Format(formatter, output_str);
  }
  fmt::format_to(std::back_inserter(output_str), " {} ", op_str);
  if (parent_precedence > GetPrecedence(b)) {
    output_str += "(";
    b.Format(formatter, output_str);
    output_str += ")";
  } else {
    b.Format(formatter, output_str);
  }
}

void PlainFormatter::Format(const Addition& expr, std::string& output) const {
  FormatBinaryOp(*this, Addition::OperatorPrecedence, "+", *expr.First(), *expr.Second(), output);
}

void PlainFormatter::Format(const Division& expr, std::string& output) const {
  FormatBinaryOp(*this, Division::OperatorPrecedence, "/", *expr.First(), *expr.Second(), output);
}

void PlainFormatter::Format(const Multiplication& expr, std::string& output) const {
  FormatBinaryOp(*this, Multiplication::OperatorPrecedence, "*", *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Power& expr, std::string& output) const {
  FormatBinaryOp(*this, Power::OperatorPrecedence, "^", *expr.First(), *expr.Second(), output);
}

void PlainFormatter::Format(const Subtraction& expr, std::string& output) const {
  FormatBinaryOp(*this, Subtraction::OperatorPrecedence, "-", *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Constant& expr, std::string& output) const {
  switch (expr.GetName()) {
    case SymbolicConstants::Pi:
      output += "pi";
      break;
    case SymbolicConstants::Euler:
      output += "e";
      break;
    default:
      output += "<UNKNOWN>";
      break;
  }
}

void PlainFormatter::Format(const Number& expr, std::string& output) const {
  fmt::format_to(std::back_inserter(output), "{}", expr.GetValue());
}

void PlainFormatter::Format(const Variable& expr, std::string& output) const {
  output += expr.GetName();
}

void PlainFormatter::Format(const NaturalLog& expr, std::string& output) const {
  output += "ln(";
  expr.Inner()->Format(*this, output);
  output += ")";
}

void PlainFormatter::Format(const Negation& expr, std::string& output) const {
  output += "-";
  expr.Inner()->Format(*this, output);
}

}  // namespace math
