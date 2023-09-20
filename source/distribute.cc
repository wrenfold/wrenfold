// Copyright 2023 Gareth Cross
#include <algorithm>

#include "assertions.h"
#include "expression_impl.h"
#include "expressions/all_expressions.h"

namespace math {

// Visitor for distributing terms in multiplications:
// (a + b) * (x + y) = a*x + a*y + b*x + b*y
struct DistributeVisitor {
  using ReturnType = Expr;

  Expr operator()(const Addition& add, const Expr&) const { return MapChildren(add, &Distribute); }
  Expr operator()(const Matrix& mat, const Expr&) const { return MapChildren(mat, &Distribute); }

  Expr operator()(const Multiplication& mul, const Expr&) const {
    // First distribute all the children of the multiplication:
    std::vector<Expr> children{};
    children.reserve(mul.Arity());
    std::transform(mul.begin(), mul.end(), std::back_inserter(children),
                   [](const Expr& expr) { return Distribute(expr); });

    // Are any of the child expressions additions?
    const std::size_t total_terms =
        std::accumulate(children.begin(), children.end(), static_cast<std::size_t>(1lu),
                        [](std::size_t total, const Expr& expr) {
                          if (const Addition* const add = CastPtr<Addition>(expr); add != nullptr) {
                            total *= add->Arity();
                          }
                          return total;
                        });

    // If the total terms is > 1, we have an addition to distribute over.
    const bool contains_additions = total_terms > 1;
    if (!contains_additions) {
      // If there are no additions, just create a new multiplication:
      // TODO: If there are no additions, and no children were altered, we could avoid this step.
      return Multiplication::FromOperands(children);
    }

    // Otherwise, we need to expand all the terms. This multiplication will become an addition of
    // multiplications. For each addition, we need to distribute its terms over the remaining
    // values. TODO: Make each of these a small vector of Expr, then convert them to multiplication.
    std::vector<Expr> output_terms(total_terms, Constants::One);

    std::size_t step = total_terms;
    for (const Expr& expr : children) {
      if (const Addition* add = CastPtr<Addition>(expr); add != nullptr) {
        // For additions, first update the step by dividing by the size of this addition:
        ASSERT_EQUAL(0, step % add->Arity());
        ASSERT_GREATER_OR_EQ(step / add->Arity(), 1);
        step /= add->Arity();
        // Now multiply terms in the addition:
        for (std::size_t out = 0; out < total_terms;) {
          for (const Expr& term : *add) {
            for (std::size_t rep = 0; rep < step; ++rep, ++out) {
              output_terms[out] = output_terms[out] * term;
            }
          }
        }
      } else {
        // Not an addition, multiply by everything:
        for (Expr& output : output_terms) {
          output = output * expr;
        }
      }
    }

    return Addition::FromOperands(output_terms);
  }

  Expr operator()(const UnaryFunction& f, const Expr&) const {
    const Expr& arg = f.Arg();
    return CreateUnaryFunction(f.Func(), Distribute(arg));
  }

  Expr operator()(const Power& pow, const Expr&) const {
    // TODO: If base is an addition, and exponent an integer, we should distribute.
    const Expr& a = pow.Base();
    const Expr& b = pow.Exponent();
    return Power::Create(Distribute(a), Distribute(b));
  }

  Expr operator()(const Conditional& conditional, const Expr&) const {
    return MapChildren(conditional, &Distribute);
  }

  Expr operator()(const Constant&, const Expr& arg) const { return arg; }
  Expr operator()(const Infinity&, const Expr& arg) const { return arg; }
  Expr operator()(const Integer&, const Expr& arg) const { return arg; }
  Expr operator()(const Float&, const Expr& arg) const { return arg; }
  Expr operator()(const FunctionArgument&, const Expr& arg) const { return arg; }
  Expr operator()(const Rational&, const Expr& arg) const { return arg; }
  Expr operator()(const Relational& relation, const Expr&) const {
    return MapChildren(relation, &Distribute);
  }
  Expr operator()(const Variable&, const Expr& arg) const { return arg; }
};

Expr Distribute(const Expr& arg) {
  return Visit(arg, [&arg](const auto& x) { return DistributeVisitor{}(x, arg); });
}

}  // namespace math
