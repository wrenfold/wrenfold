#include "functions.h"

#include "binary_operations.h"
#include "constant_expressions.h"
#include "expr.h"
#include "unary_operations.h"

namespace math {

// TODO(gareth): Throw on invalid values...
Expr log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  return MakeExpr<NaturalLog>(x.GetImpl());
}

Expr pow(const Expr& x, const Expr& y) { return Expr{Pow(x.GetImpl(), y.GetImpl())}; }

}  // namespace math
