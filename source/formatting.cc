#include "formatting.h"

#include <fmt/format.h>

#include <iostream>
#include <optional>
#include <type_traits>

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

static int GetPrecedence(const Expr& expr) {
  const OperationBase* const op = expr.GetRaw<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

// Helper for appropriately applying brackets, considering operator precedence.
void PlainFormatter::FormatBinaryOp(const int parent_precedence, const char* const op_str,
                                    const Expr& a, const Expr& b) {
  if (parent_precedence > GetPrecedence(a) || !use_precedence_) {
    output_ += "(";
    a.Receive(*this);
    output_ += ")";
  } else {
    a.Receive(*this);
  }
  fmt::format_to(std::back_inserter(output_), " {} ", op_str);
  if (parent_precedence > GetPrecedence(b) || !use_precedence_) {
    output_ += "(";
    b.Receive(*this);
    output_ += ")";
  } else {
    b.Receive(*this);
  }
}

void PlainFormatter::Apply(const Addition& expr) {
  FormatBinaryOp(Addition::OperatorPrecedence, "+", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Constant& expr) {
  switch (expr.GetName()) {
    case SymbolicConstants::Pi:
      output_ += "pi";
      break;
    case SymbolicConstants::Euler:
      output_ += "e";
      break;
    default:
      output_ += "<UNKNOWN CONSTANT>";
      break;
  }
}

void PlainFormatter::Apply(const Division& expr) {
  FormatBinaryOp(Division::OperatorPrecedence, "/", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Multiplication& expr) {
  FormatBinaryOp(Multiplication::OperatorPrecedence, "*", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner().Receive(*this);
  output_ += ")";
}

struct NegateNeedsBracketsVisitor {
  using ReturnType = bool;

  template <typename Derived>
  constexpr bool Apply(const BinaryOp<Derived>&) const {
    return true;
  }
};

void PlainFormatter::Apply(const Negation& expr) {
  output_ += "-";

  const std::optional<bool> needs_brackets = Visit(expr.Inner(), NegateNeedsBracketsVisitor{});
  if (needs_brackets.value_or(false)) {
    output_ += "(";
    expr.Inner().Receive(*this);
    output_ += ")";
  } else {
    expr.Inner().Receive(*this);
  }
}

void PlainFormatter::Apply(const Number& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Power& expr) {
  output_ += "pow(";
  expr.First().Receive(*this);
  output_ += ", ";
  expr.Second().Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Subtraction& expr) {
  FormatBinaryOp(Subtraction::OperatorPrecedence, "-", expr.First(), expr.Second());
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

}  // namespace math
