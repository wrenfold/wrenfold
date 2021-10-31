#include "binary_operations.h"

namespace math {

static int GetPrecedence(const ExpressionBase& expr) {
  const OperationBase* const op = expr.As<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

static std::string FormatString(const int parent_precedence, const std::string& op_str,
                                const ExpressionBase& a, const ExpressionBase& b) {
  std::string result;
  result.reserve(32);
  if (parent_precedence > GetPrecedence(a)) {
    fmt::format_to(std::back_inserter(result), "({})", a.Format());
  } else {
    result += a.Format();
  }
  fmt::format_to(std::back_inserter(result), " {} ", op_str);
  if (parent_precedence > GetPrecedence(b)) {
    fmt::format_to(std::back_inserter(result), "({})", b.Format());
  } else {
    result += b.Format();
  }
  return result;
}

ExpressionBaseConstPtr Addition::Diff(const Variable& var) const {
  return CreateAddition(a_->Diff(var), b_->Diff(var));
}

std::string Addition::Format() const { return FormatString(OperatorPrecedence, "+", *a_, *b_); }

ExpressionBaseConstPtr Subtraction::Diff(const Variable& var) const {
  return CreateSubtraction(a_->Diff(var), b_->Diff(var));
}

std::string Subtraction::Format() const { return FormatString(OperatorPrecedence, "-", *a_, *b_); }

ExpressionBaseConstPtr Multiplication::Diff(const Variable& var) const {
  return CreateAddition(CreateMultiplication(a_, b_->Diff(var)),
                        CreateMultiplication(a_->Diff(var), b_));
}

std::string Multiplication::Format() const {
  return FormatString(OperatorPrecedence, "*", *a_, *b_);
}

}  // namespace math
