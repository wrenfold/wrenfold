// Copyright 2023 Gareth Cross
#include "expressions/conditional.h"

#include "constants.h"
#include "expressions/all_expressions.h"
#include "visitor_impl.h"

namespace math {

// Visitor that eliminates redundant conditionals.
class ConditionalSimplificationVisitor {
 public:
  using ReturnType = Expr;

  ConditionalSimplificationVisitor(const Expr& condition, bool value)
      : condition_(condition), value_(value) {}

  // TODO: Cannot yet simplify when a condition implies the opposite of another condition.
  Expr ApplyConditional(const Conditional& cond) {
    if (cond.Condition().IsIdenticalTo(condition_)) {
      if (value_) {
        return Visit(cond.IfBranch(),
                     [this, &cond](const auto& x) { return this->operator()(x, cond.IfBranch()); });
      } else {
        return Visit(cond.ElseBranch(), [this, &cond](const auto& x) {
          return this->operator()(x, cond.ElseBranch());
        });
      }
    }
    return MapChildren(cond, [this](const Expr& x) {
      return Visit(x, [this, &x](const auto& y) { return operator()(y, x); });
    });
  }

  template <typename T>
  Expr operator()(const T& thing, const Expr& expr) {
    if constexpr (std::is_same_v<T, Conditional>) {
      return ApplyConditional(thing);
    } else if constexpr (T::IsLeafNode) {
      return expr;
    } else {
      return MapChildren(thing, [this](const Expr& x) {
        return Visit(x, [this, &x](const auto& y) { return operator()(y, x); });
      });
    }
  }

 private:
  const Expr& condition_;
  bool value_;
};

Expr Conditional::Create(math::Expr condition, math::Expr if_branch, math::Expr else_branch) {
  if (condition.IsIdenticalTo(Constants::True)) {
    return if_branch;
  } else if (condition.IsIdenticalTo(Constants::False)) {
    return else_branch;
  }

  // Check for redundancies and eliminate them:
  Expr if_branch_simplified =
      VisitWithExprArg(if_branch, ConditionalSimplificationVisitor{condition, true});
  Expr else_branch_simplified =
      VisitWithExprArg(else_branch, ConditionalSimplificationVisitor{condition, false});

  if (if_branch_simplified.IsIdenticalTo(else_branch_simplified)) {
    return if_branch_simplified;
  }
  return MakeExpr<Conditional>(std::move(condition), std::move(if_branch_simplified),
                               std::move(else_branch_simplified));
}

}  // namespace math