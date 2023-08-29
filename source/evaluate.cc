// Copyright 2023 Gareth Cross
#include <algorithm>

#include <math.h>

#include "expression.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

struct EvaluateVisitor {
  using ReturnType = Expr;

  Expr operator()(const Addition& add, const Expr&) const { return MapChildren(add, &Eval); }
  Expr operator()(const Matrix& mat, const Expr&) const { return MapChildren(mat, &Eval); }
  Expr operator()(const Multiplication& mul, const Expr&) const { return MapChildren(mul, &Eval); }
  Expr operator()(const UnaryFunction& f, const Expr&) const { return MapChildren(f, &Eval); }
  Expr operator()(const Power& pow, const Expr&) const { return MapChildren(pow, &Eval); }
  Expr operator()(const Conditional& cond, const Expr&) const { return MapChildren(cond, &Eval); }

  Expr operator()(const Constant& c, const Expr&) const {
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
  Expr operator()(const Infinity&, const Expr&) const {
    throw TypeError("Cannot evaluate complex infinity to float.");
  }
  Expr operator()(const Integer& i, const Expr&) const {
    return MakeExpr<Float>(static_cast<Float>(i));
  }
  Expr operator()(const Float&, const Expr& arg) const { return arg; }
  Expr operator()(const FunctionArgument&, const Expr& arg) const { return arg; }
  Expr operator()(const Rational& r, const Expr&) const {
    return Float::Create(static_cast<Float>(r));
  }
  Expr operator()(const Relational& r, const Expr&) const { return MapChildren(r, &Eval); }
  Expr operator()(const Variable&, const Expr& arg) const { return arg; }
};

Expr Eval(const Expr& arg) { return Visit(arg, EvaluateVisitor{}, arg); }

}  // namespace math
