#include "formatting.h"

#include <fmt/format.h>
#include <fmt/xchar.h>

#include <codecvt>
#include <locale>

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

StringType ToPlainString(const ExpressionBaseConstPtr& expr) {
  PlainFormatter formatter{};
  StringType result;
  expr->Format(formatter, result);
  return result;
}

std::string NarrowFromWide(const std::wstring& wide_str) {
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converter;
  return converter.to_bytes(wide_str);
}

std::wstring WideFromNarrow(const std::string& str) {
  std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
  return converter.from_bytes(str);
}

std::string ToPlainNarrowString(const ExpressionBaseConstPtr& expr) {
#ifdef USE_WIDE_STR
  const StringType wide_str = ToPlainString(expr);
  return NarrowFromWide(wide_str);
#else
  return ToPlainString(expr);
#endif
}

static int GetPrecedence(const ExpressionBase& expr) {
  const OperationBase* const op = expr.As<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

// Helper for appropriately applying brackets, considering operator precedence.
static void FormatBinaryOp(const Formatter& formatter, const int parent_precedence,
                           const CharType* const op_str, const ExpressionBase& a,
                           const ExpressionBase& b, StringType& output_str) {
  if (parent_precedence > GetPrecedence(a)) {
    output_str += TEXT("(");
    a.Format(formatter, output_str);
    output_str += TEXT(")");
  } else {
    a.Format(formatter, output_str);
  }
  fmt::format_to(std::back_inserter(output_str), TEXT(" {} "), op_str);
  if (parent_precedence > GetPrecedence(b)) {
    output_str += TEXT("(");
    b.Format(formatter, output_str);
    output_str += TEXT(")");
  } else {
    b.Format(formatter, output_str);
  }
}

void PlainFormatter::Format(const Addition& expr, StringType& output) const {
  FormatBinaryOp(*this, Addition::OperatorPrecedence, TEXT("+"), *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Division& expr, StringType& output) const {
  FormatBinaryOp(*this, Division::OperatorPrecedence, TEXT("/"), *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Multiplication& expr, StringType& output) const {
  FormatBinaryOp(*this, Multiplication::OperatorPrecedence, TEXT("*"), *expr.First(),
                 *expr.Second(), output);
}

void PlainFormatter::Format(const Power& expr, StringType& output) const {
  FormatBinaryOp(*this, Power::OperatorPrecedence, TEXT("^"), *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Subtraction& expr, StringType& output) const {
  FormatBinaryOp(*this, Subtraction::OperatorPrecedence, TEXT("-"), *expr.First(), *expr.Second(),
                 output);
}

void PlainFormatter::Format(const Constant& expr, StringType& output) const {
  switch (expr.GetName()) {
    case SymbolicConstants::Pi:
      output += TEXT("pi");
      break;
    case SymbolicConstants::Euler:
      output += TEXT("e");
      break;
    default:
      output += TEXT("<UNKNOWN>");
      break;
  }
}

void PlainFormatter::Format(const Number& expr, StringType& output) const {
  fmt::format_to(std::back_inserter(output), TEXT("{}"), expr.GetValue());
}

void PlainFormatter::Format(const Variable& expr, StringType& output) const {
  output += expr.GetName();
}

void PlainFormatter::Format(const NaturalLog& expr, StringType& output) const {
  output += TEXT("ln(");
  expr.Inner()->Format(*this, output);
  output += TEXT(")");
}

void PlainFormatter::Format(const Negate& expr, StringType& output) const {
  output += TEXT("-");
  expr.Inner()->Format(*this, output);
}

}  // namespace math
