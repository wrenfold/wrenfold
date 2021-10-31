#include "constant.h"
#include "numeric_constants.h"

#include <fmt/format.h>

namespace math {

ExpressionBaseConstPtr Constant::Diff(const Variable&) const { return NumericConstants::Zero; }

std::string Constant::ToString() const { return fmt::format("{}", val_); }

}  // namespace math
