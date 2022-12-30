#include "constants.h"

#include "constant_expressions.h"
#include "expression.h"

namespace math {

const Expr Constants::Zero = MakeExpr<Integer>(0);
const Expr Constants::One = MakeExpr<Integer>(1);
const Expr Constants::Pi = MakeExpr<Constant>(SymbolicConstants::Pi);
const Expr Constants::Euler = MakeExpr<Constant>(SymbolicConstants::Euler);

}  // namespace math
