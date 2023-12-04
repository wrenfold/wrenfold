#include "constants.h"

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace math {

const Expr constants::zero = make_expr<integer_constant>(0);
const Expr constants::one = make_expr<integer_constant>(1);
const Expr constants::pi = make_expr<Constant>(symbolic_constants::pi);
const Expr constants::euler = make_expr<Constant>(symbolic_constants::euler);
const Expr constants::negative_one = make_expr<integer_constant>(-1);
const Expr constants::complex_infinity = make_expr<Infinity>();
const Expr constants::boolean_true = make_expr<Constant>(symbolic_constants::boolean_true);
const Expr constants::boolean_false = make_expr<Constant>(symbolic_constants::boolean_false);
const Expr constants::undefined = make_expr<math::Undefined>();

}  // namespace math
