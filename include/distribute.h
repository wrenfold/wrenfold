// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "expression_impl.h"
#include "expressions/all_expressions.h"

namespace math {

struct DistributeVisitor {
  using ReturnType = Expr;
  static constexpr VisitorPolicy Policy = VisitorPolicy::CompileError;

  Expr Apply(const Addition& add) const {
    std::vector<Expr> args{};
    args.reserve(add.Arity());
    for (std::size_t i = 0; i < add.Arity(); ++i) {
      std::optional<Expr> distributed = VisitStruct(add[i], DistributeVisitor{add[i]});
      ASSERT(distributed.has_value());
      args.push_back(std::move(*distributed));
    }
    return Addition::FromOperands(args);
  }

  Expr Apply(const Constant&) const { return arg_; }

  Expr Apply(const Multiplication& mul) const {
    // First distribute all the children of the multiplication:
    std::vector<Expr> children{};
    children.reserve(mul.Arity());
    std::transform(mul.begin(), mul.end(), std::back_inserter(children), [](const Expr& expr) {
      return VisitStruct(expr, DistributeVisitor{expr}).value();
    });

    // Are any of the child expressions additions?
    const std::size_t total_terms = std::accumulate(
        children.begin(), children.end(), 1lu, [](std::size_t total, const Expr& expr) {
          if (const Addition* const add = TryCast<Addition>(expr); add != nullptr) {
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
      if (const Addition* add = TryCast<Addition>(expr); add != nullptr) {
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

  Expr Apply(const NaturalLog& log) {
    std::optional<Expr> inner = VisitStruct(log.Inner(), DistributeVisitor{log.Inner()});
    return NaturalLog::Create(inner.value());
  }

  Expr Apply(const Integer&) { return arg_; }
  Expr Apply(const Float&) { return arg_; }

  Expr Apply(const Power& pow) {
    const Expr& a = pow.Base();
    const Expr& b = pow.Exponent();
    return Power::Create(VisitStruct(a, DistributeVisitor{a}).value(),
                         VisitStruct(b, DistributeVisitor{b}).value());
  }

  Expr Apply(const Rational&) const { return arg_; }
  Expr Apply(const Variable&) { return arg_; }

  explicit DistributeVisitor(const Expr& arg) : arg_(arg) {}

  const Expr& arg_;
};

}  // namespace math
