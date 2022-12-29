#include "functions.h"

#include "operations_inline.h"

namespace math {

Expr log(const Expr& x) { return NaturalLog::Create(x); }

Expr pow(const Expr& x, const Expr& y) { return Power::Create(x, y); }

}  // namespace math
