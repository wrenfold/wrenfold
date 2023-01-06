#include "constants.h"

#include "expression.h"
#include "expressions/numeric_expressions.h"
#include "expressions/special_constants.h"

namespace math {

const Expr Constants::Zero = MakeExpr<Integer>(0);
const Expr Constants::One = MakeExpr<Integer>(1);
const Expr Constants::Pi = MakeExpr<Constant>(SymbolicConstants::Pi);
const Expr Constants::Euler = MakeExpr<Constant>(SymbolicConstants::Euler);
const Expr Constants::NegativeOne = MakeExpr<Integer>(-1);

}  // namespace math
