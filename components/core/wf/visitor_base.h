// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

#include "wf/enumerations.h"
#include "wf/template_utils.h"

namespace math {

class Expr;
class expression_concept;

// TODO: Allow switching this out for something w/o atomic operations?
using expression_concept_const_ptr = std::shared_ptr<const class expression_concept>;

// clang-format off
using ExpressionTypeList = type_list<
    class addition,
    class cast_bool,
    class conditional,
    class Constant,
    class derivative,
    class float_constant,
    class function,
    class Infinity,
    class integer_constant,
    class multiplication,
    class power,
    class rational_constant,
    class relational,
    class Undefined,
    class Variable
    >;
// clang-format on

}  // namespace math
