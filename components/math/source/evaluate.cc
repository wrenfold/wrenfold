// Copyright 2023 Gareth Cross
#include "expression.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

struct EvaluateVisitor {
  Expr operator()(const Addition& add, const Expr&) const { return add.map_children(&evaluate); }
  Expr operator()(const Multiplication& mul, const Expr&) const {
    return mul.map_children(&evaluate);
  }
  Expr operator()(const Function& f, const Expr&) const { return f.map_children(&evaluate); }
  Expr operator()(const Power& pow, const Expr&) const { return pow.map_children(&evaluate); }
  Expr operator()(const Conditional& cond, const Expr&) const {
    return cond.map_children(&evaluate);
  }

  Expr operator()(const CastBool& cast) const {
    Expr eval = cast.map_children(&evaluate);
    if (!eval.is_type<CastBool>()) {
      return evaluate(eval);
    }
    return eval;
  }

  Expr operator()(const Constant& c) const {
    const double value = double_from_symbolic_constant(c.name());
    ZEN_ASSERT(!std::isnan(value), "Invalid symbolic constant: {}",
               string_from_symbolic_constant(c.name()));
    return Float::create(value);
  }

  Expr operator()(const Derivative& d, const Expr&) const { return d.map_children(&evaluate); }

  Expr operator()(const Infinity&) const {
    throw TypeError("Cannot evaluate complex infinity to float.");
  }
  Expr operator()(const Integer& i) const { return make_expr<Float>(static_cast<Float>(i)); }
  Expr operator()(const Float&, const Expr& arg) const { return arg; }
  Expr operator()(const Rational& r) const { return Float::create(static_cast<Float>(r)); }
  Expr operator()(const Relational& r) const { return r.map_children(&evaluate); }
  Expr operator()(const Undefined&) const { return Constants::Undefined; }
  Expr operator()(const Variable&, const Expr& arg) const { return arg; }
};

Expr evaluate(const Expr& arg) { return visit_with_expr(arg, EvaluateVisitor{}); }

}  // namespace math
