#include "constants.h"

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"

namespace math {

const Expr constants::zero = make_expr<Integer>(0);
const Expr constants::one = make_expr<Integer>(1);
const Expr constants::pi = make_expr<Constant>(SymbolicConstants::Pi);
const Expr constants::euler = make_expr<Constant>(SymbolicConstants::Euler);
const Expr constants::negative_one = make_expr<Integer>(-1);
const Expr constants::complex_infinity = make_expr<Infinity>();
const Expr constants::boolean_true = make_expr<Constant>(SymbolicConstants::True);
const Expr constants::boolean_false = make_expr<Constant>(SymbolicConstants::False);
const Expr constants::undefined = make_expr<math::Undefined>();

}  // namespace math
