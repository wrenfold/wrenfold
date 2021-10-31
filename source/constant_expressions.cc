#include "constant_expressions.h"

#include <fmt/format.h>

#include "assertions.hpp"
#include "constants.h"

namespace math {

ExpressionBaseConstPtr Number::Diff(const Variable&) const { return Constants::Zero; }

std::string Number::Format() const { return fmt::format("{}", val_); }

ExpressionBaseConstPtr Constant::Diff(const Variable&) const { return Constants::Zero; }

std::string Constant::Format() const {
  switch (name_) {
    case SymbolicConstants::Pi:
      return "pi";
    case SymbolicConstants::Euler:
      return "e";
    default:
      break;
  }
  return "<unknown>";
}

}  // namespace math
