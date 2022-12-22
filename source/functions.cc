#include "functions.h"

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"

namespace math {

// TODO(gareth): Throw on invalid values...
Expr log(const Expr& x) {
  if (x.IsIdenticalTo(Constants::Euler)) {
    return Constants::One;
  }
  return MakeExpr<NaturalLog>(x);
}

Expr pow(const Expr& x, const Expr& y) { return MakeExpr<Power>(x, y); }

}  // namespace math
