#include "constants.h"

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace wf {

const scalar_expr constants::zero = make_expr<integer_constant>(0);
const scalar_expr constants::one = make_expr<integer_constant>(1);
const scalar_expr constants::pi = make_expr<symbolic_constant>(symbolic_constant_enum::pi);
const scalar_expr constants::euler = make_expr<symbolic_constant>(symbolic_constant_enum::euler);
const scalar_expr constants::negative_one = make_expr<integer_constant>(-1);
const scalar_expr constants::complex_infinity = make_expr<wf::complex_infinity>();
const scalar_expr constants::undefined = make_expr<wf::undefined>();

const boolean_expr constants::boolean_true{std::in_place_type_t<boolean_constant>{}, true};
const boolean_expr constants::boolean_false{std::in_place_type_t<boolean_constant>{}, false};

}  // namespace wf
