// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

namespace math {

class Expr;
class ExpressionConcept;

class VisitorWithResultBase;
class VisitorWithoutResultBase;

// TODO: Allow switching this out for something w/o atomic operations?
using ExpressionConceptConstPtr = std::shared_ptr<const ExpressionConcept>;

}  // namespace math
