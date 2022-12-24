#include "functions.h"

#include "expr.h"
#include "operation_utils.h"

namespace math {

Expr log(const Expr& x) { return Expr{Log(x.GetImpl())}; }

Expr pow(const Expr& x, const Expr& y) { return Expr{Pow(x.GetImpl(), y.GetImpl())}; }

}  // namespace math
