// Copyright 2023 Gareth Cross
#pragma once
#include "wf/assertions.h"
#include "wf/expression.h"

namespace wf {

// Expression for expressing an unevaluated derivative operation.
// For example df(x)/dx is `Derivative(f(x), x)`. We use this to represent derivatives that can't
// be immediately evaluated.
class derivative {
 public:
  static constexpr std::string_view name_str = "Derivative";
  static constexpr bool is_leaf_node = false;

  derivative(scalar_expr differentiand, scalar_expr arg, int order = 1)
      : children_{std::move(differentiand), std::move(arg)}, order_(order) {
    WF_ASSERT_GREATER_OR_EQ(order_, 1);
  }

  // The function we are taking the derivative of:
  constexpr const scalar_expr& differentiand() const noexcept { return children_[0]; }

  // The variable with respect to which the derivative is being taken.
  constexpr const scalar_expr& argument() const noexcept { return children_[1]; }

  // Order of the derivative (first, second, third, etc).
  constexpr int order() const noexcept { return order_; }

  // Access children as iterator.
  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

  // All arguments must match.
  bool is_identical_to(const derivative& other) const {
    return order_ == other.order_ &&
           std::equal(begin(), end(), other.begin(), is_identical_struct<scalar_expr>{});
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return derivative::create(operation(differentiand()), operation(argument()), order_);
  }

  // Create a new derivative expression.
  static scalar_expr create(scalar_expr function, scalar_expr arg, int order);

 private:
  std::array<scalar_expr, 2> children_;
  int order_;
};

template <>
struct hash_struct<derivative> {
  std::size_t operator()(const derivative& func) const {
    return hash_args(static_cast<std::size_t>(func.order()), func.differentiand(), func.argument());
  }
};

}  // namespace wf
