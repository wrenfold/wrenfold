#include "unary_operations.h"

#include <fmt/format.h>

namespace math {

ExpressionBaseConstPtr Negate::Diff(const Variable& var) const {
  return CreateNegation(x_->Diff(var));
}

std::string Negate::Format() const {
  return fmt::format("-{}", x_->Format());
}

ExpressionBaseConstPtr NaturalLog::Diff(const Variable&) const {
  return {};
}

std::string NaturalLog::Format() const {
  return fmt::format("ln({})", x_->Format());
}

} // namespace math
