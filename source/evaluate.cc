// Copyright 2023 Gareth Cross
#include <algorithm>

#include <math.h>

#include "expression.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

struct EvaluateVisitor {
  using ReturnType = Expr;

  Expr Apply(const Addition& add, const Expr&) const { return MapChildren(add, &Eval); }
  Expr Apply(const Matrix& mat, const Expr&) const { return MapChildren(mat, &Eval); }
  Expr Apply(const Multiplication& mul, const Expr&) const { return MapChildren(mul, &Eval); }
  Expr Apply(const UnaryFunction& f, const Expr&) const { return MapChildren(f, &Eval); }
  Expr Apply(const Power& pow, const Expr&) const { return MapChildren(pow, &Eval); }
  Expr Apply(const Conditional& cond, const Expr&) const { return MapChildren(cond, &Eval); }

  Expr Apply(const Constant& c, const Expr&) const {
    switch (c.GetName()) {
      case SymbolicConstants::Euler:
        return Float::Create(std::exp(1.0));
      case SymbolicConstants::Pi:
        return Float::Create(M_PI);
      case SymbolicConstants::True:
        return Constants::One;
      case SymbolicConstants::False:
        return Constants::Zero;
      default:
        break;
    }
    ASSERT(false, "Invalid symbolic constant: {}", StringFromSymbolicConstant(c.GetName()));
    return Float::Create(std::numeric_limits<double>::quiet_NaN());
  }
  Expr Apply(const Infinity&, const Expr&) const {
    throw TypeError("Cannot evaluate complex infinity to float.");
  }
  Expr Apply(const Integer& i, const Expr&) const { return MakeExpr<Float>(static_cast<Float>(i)); }
  Expr Apply(const Float&, const Expr& arg) const { return arg; }
  Expr Apply(const FunctionArgument&, const Expr& arg) const { return arg; }
  Expr Apply(const Rational& r, const Expr&) const { return Float::Create(static_cast<Float>(r)); }
  Expr Apply(const Relational& r, const Expr&) const { return MapChildren(r, &Eval); }
  Expr Apply(const Variable&, const Expr& arg) const { return arg; }
};

Expr Eval(const Expr& arg) { return VisitStruct(arg, EvaluateVisitor{}, arg); }

}  // namespace math
