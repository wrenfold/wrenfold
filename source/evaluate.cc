// Copyright 2023 Gareth Cross
#include <algorithm>

#include "expression.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

struct EvaluateVisitor {
  using ReturnType = Expr;
  using Policy = VisitorPolicy::CompileError;

  Expr Apply(const Addition& add) const { return MapChildren(add, &Eval); }
  Expr Apply(const Matrix& mat) const { return MapChildren(mat, &Eval); }
  Expr Apply(const Multiplication& mul) const { return MapChildren(mul, &Eval); }
  Expr Apply(const Expr&, const UnaryFunction& f) const { return MapChildren(f, &Eval); }
  Expr Apply(const Expr&, const Power& pow) const { return MapChildren(pow, &Eval); }

  Expr Apply(const Constant& c) const {
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
  Expr Apply(const Infinity&) const {
    throw TypeError("Cannot evaluate complex infinity to float.");
  }
  Expr Apply(const Expr&, const Integer& i) const { return MakeExpr<Float>(static_cast<Float>(i)); }
  Expr Apply(const Expr& arg, const Float&) const { return arg; }
  Expr Apply(const Expr& arg, const FunctionArgument&) const { return arg; }
  Expr Apply(const Rational& r) const { return Float::Create(static_cast<Float>(r)); }
  Expr Apply(const Relational& r) const { return MapChildren(r, &Eval); }
  Expr Apply(const Expr& arg, const Variable&) const { return arg; }
};

Expr Eval(const Expr& arg) { return VisitStruct(arg, EvaluateVisitor{}); }

}  // namespace math
