#include "functions.h"

#include "operations_inline.h"

namespace math {

Expr log(const Expr& x) { return Log(x); }

Expr pow(const Expr& x, const Expr& y) { return Pow(x, y); }

}  // namespace math
