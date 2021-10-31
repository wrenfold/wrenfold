#include "constants.h"

#include "constant_expressions.h"  //  for MakeNum

namespace math {

const Expr Constants::Zero = MakeExpr<Number>(0);
const Expr Constants::One = MakeExpr<Number>(1);
const Expr Constants::Pi = MakeExpr<Constant>(SymbolicConstants::Pi);
const Expr Constants::Euler = MakeExpr<Constant>(SymbolicConstants::Euler);

bool IsZero(const ExpressionBaseConstPtr& expr) { return expr->IsIdenticalTo(Constants::Zero); }

bool IsOne(const ExpressionBaseConstPtr& expr) { return expr->IsIdenticalTo(Constants::One); }

}  // namespace math
