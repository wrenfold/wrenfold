// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

namespace math {

class Expr;
class ExpressionBase;

// TODO: Allow switching this out for something w/o atomic operations?
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

}  // namespace math
