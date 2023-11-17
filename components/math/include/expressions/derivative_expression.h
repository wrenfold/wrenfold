// Copyright 2023 Gareth Cross
#pragma once
#include "expression.h"
#include "expression_impl.h"

#include "assertions.h"

namespace math {

// Expression for expressing an unevaluated derivative operation.
// For example df(x)/dx is `Derivative(f(x), x)`. We use this to represent derivatives that can't
// be immediately evaluated.
class Derivative {
 public:
  static constexpr std::string_view NameStr = "Derivative";
  static constexpr bool IsLeafNode = false;

  Derivative(Expr differentiand, Expr arg, int order = 1)
      : children_{std::move(differentiand), std::move(arg)}, order_(order) {
    ZEN_ASSERT_GREATER_OR_EQ(order_, 1);
  }

  // The function we are taking the derivative of:
  constexpr const Expr& differentiand() const noexcept { return children_[0]; }

  // The variable with respect to which the derivative is being taken.
  constexpr const Expr& argument() const noexcept { return children_[1]; }

  // Order of the derivative (first, second, third, etc).
  constexpr int order() const noexcept { return order_; }

  // Access children as iterator.
  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

  // All arguments must match.
  bool is_identical_to(const Derivative& other) const {
    return order_ == other.order_ &&
           std::equal(begin(), end(), other.begin(), is_identical_struct<Expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    return Derivative::create(operation(differentiand()), operation(argument()), order_);
  }

  // Create a new derivative expression.
  static Expr create(Expr differentiand, Expr arg, int order);

 private:
  std::array<Expr, 2> children_;
  int order_;
};

template <>
struct hash_struct<Derivative> {
  std::size_t operator()(const Derivative& func) const {
    return hash_args(static_cast<std::size_t>(func.order()), func.differentiand(), func.argument());
  }
};

}  // namespace math
