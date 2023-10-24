#include "constants.h"

#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "expressions/special_constants.h"

namespace math {

const Expr Constants::Zero = make_expr<Integer>(0);
const Expr Constants::One = make_expr<Integer>(1);
const Expr Constants::Pi = make_expr<Constant>(SymbolicConstants::Pi);
const Expr Constants::Euler = make_expr<Constant>(SymbolicConstants::Euler);
const Expr Constants::NegativeOne = make_expr<Integer>(-1);
const Expr Constants::ComplexInfinity = make_expr<Infinity>();
const Expr Constants::True = make_expr<Constant>(SymbolicConstants::True);
const Expr Constants::False = make_expr<Constant>(SymbolicConstants::False);
const Expr Constants::Undefined = make_expr<math::Undefined>();

}  // namespace math
