// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

#include "wf/enumerations.h"
#include "wf/template_utils.h"

namespace math {

class Expr;
class ExpressionConcept;

// TODO: Allow switching this out for something w/o atomic operations?
using expression_concept_const_ptr = std::shared_ptr<const class ExpressionConcept>;

// clang-format off
using ExpressionTypeList = type_list<
    class Addition,
    class CastBool,
    class Conditional,
    class Constant,
    class Derivative,
    class Float,
    class Function,
    class Infinity,
    class Integer,
    class Multiplication,
    class Power,
    class Rational,
    class Relational,
    class Undefined,
    class Variable
    >;
// clang-format on

}  // namespace math
