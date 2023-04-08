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
  using Policy = VisitorPolicy::CompileError;

  Expr Apply(const Expr&, const Addition& add) const {
    std::vector<Expr> args{};
    args.reserve(add.Arity());
    for (std::size_t i = 0; i < add.Arity(); ++i) {
      Expr distributed = Distribute(add[i]);
      args.push_back(std::move(distributed));
    }
    return Addition::FromOperands(args);
  }

  Expr Apply(const Expr&, const Matrix& mat) const {
    std::vector<Expr> output;
    output.reserve(mat.Size());
    std::transform(mat.begin(), mat.end(), std::back_inserter(output),
                   [](const Expr& x) { return Distribute(x); });
    return MakeExpr<Matrix>(mat.NumRows(), mat.NumCols(), std::move(output));
  }

  Expr Apply(const Expr&, const Multiplication& mul) const {
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

  Expr Apply(const Expr&, const UnaryFunction& f) const {
    const Expr& arg = f.Arg();
    return CreateUnaryFunction(f.Func(), Distribute(arg));
  }

  Expr Apply(const Expr&, const Power& pow) const {
    // TODO: If base is an addition, and exponent an integer, we should distribute.
    const Expr& a = pow.Base();
    const Expr& b = pow.Exponent();
    return Power::Create(Distribute(a), Distribute(b));
  }

  Expr Apply(const Expr& arg, const Constant&) const { return arg; }
  Expr Apply(const Expr& arg, const Integer&) const { return arg; }
  Expr Apply(const Expr& arg, const Float&) const { return arg; }
  Expr Apply(const Expr& arg, const FunctionArgument&) const { return arg; }
  Expr Apply(const Expr& arg, const Rational&) const { return arg; }
  Expr Apply(const Expr& arg, const Variable&) const { return arg; }
};

Expr Distribute(const Expr& arg) { return VisitStruct(arg, DistributeVisitor{}); }

}  // namespace math
