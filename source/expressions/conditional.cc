// Copyright 2023 Gareth Cross
#include "expressions/conditional.h"

#include "constants.h"

namespace math {

Expr Conditional::Create(math::Expr condition, math::Expr if_branch, math::Expr else_branch) {
  if (condition.IsIdenticalTo(Constants::True)) {
    return if_branch;
  } else if (condition.IsIdenticalTo(Constants::False)) {
    return else_branch;
  }
  return MakeExpr<Conditional>(std::move(condition), std::move(if_branch), std::move(else_branch));
}

}  // namespace math
