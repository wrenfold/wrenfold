// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

#include "wf/enumerations.h"
#include "wf/template_utils.h"

namespace wf {

class Expr;
class expression_concept;

// TODO: Allow switching this out for something w/o atomic operations?
using expression_concept_const_ptr = std::shared_ptr<const class expression_concept>;

// clang-format off
using expression_type_list = type_list<
    class addition,
    class cast_bool,
    class conditional,
    class symbolic_constant,
    class derivative,
    class float_constant,
    class function,
    class complex_infinity,
    class integer_constant,
    class multiplication,
    class power,
    class rational_constant,
    class relational,
    class undefined,
    class variable
    >;


using trivial_type_list = type_list<
    class float_constant,
    class integer_constant,
    class rational_constant
    >;

using non_trivial_type_list = type_list<
    class addition,
class cast_bool,
class conditional,
class symbolic_constant,
class derivative,
class function,
class complex_infinity,
class multiplication,
class power,
class relational,
class undefined,
class variable
    >;
// clang-format on

}  // namespace wf
