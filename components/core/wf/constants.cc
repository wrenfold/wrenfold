// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "constants.h"

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace wf {

const scalar_expr constants::zero{std::in_place_type_t<wf::integer_constant>{}, 0};
const scalar_expr constants::one{std::in_place_type_t<wf::integer_constant>{}, 1};
const scalar_expr constants::pi{std::in_place_type_t<wf::symbolic_constant>{},
                                symbolic_constant_enum::pi};
const scalar_expr constants::euler{std::in_place_type_t<wf::symbolic_constant>{},
                                   symbolic_constant_enum::euler};
const scalar_expr constants::negative_one{std::in_place_type_t<wf::integer_constant>{}, -1};
const scalar_expr constants::complex_infinity{std::in_place_type_t<wf::complex_infinity>{}};
const scalar_expr constants::undefined{std::in_place_type_t<wf::undefined>{}};
const scalar_expr constants::imaginary_unit{std::in_place_type_t<wf::imaginary_unit>{}};

const scalar_expr constants::two{std::in_place_type_t<wf::integer_constant>{}, 2};
const scalar_expr constants::negative_two{std::in_place_type_t<wf::integer_constant>{}, -2};

const boolean_expr constants::boolean_true{std::in_place_type_t<boolean_constant>{}, true};
const boolean_expr constants::boolean_false{std::in_place_type_t<boolean_constant>{}, false};

}  // namespace wf
