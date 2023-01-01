// Copyright 2022 Gareth Cross
#pragma once
#include "constants.h"
#include "operation_bases.h"
#include "visitor_impl.h"

namespace math {

class Addition : public NAryOp<Addition> {
 public:
  using NAryOp::NAryOp;
  static constexpr const char* NameStr = "Addition";

  // Construct from two operands.
  // Templated so we can forward r-value references.
  static Expr FromTwoOperands(const Expr& a, const Expr& b) {
    // Check if either argument is zero:
    if (IsZero(a)) {
      return b;
    } else if (IsZero(b)) {
      return a;
    }

    // TODO: Clean this up...
    const Addition* a_add = TryCast<Addition>(a);
    const Addition* b_add = TryCast<Addition>(b);

    std::vector<Expr> args;
    args.reserve(2);
    if (a_add) {
      args.insert(args.end(), a_add->args_.begin(), a_add->args_.end());
    } else {
      args.push_back(a);
    }
    if (b_add) {
      args.insert(args.end(), b_add->args_.begin(), b_add->args_.end());
    } else {
      args.push_back(b);
    }
    return MakeExpr<Addition>(std::move(args));
  }
};

}  // namespace math
