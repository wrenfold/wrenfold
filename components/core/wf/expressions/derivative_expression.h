// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"
#include "wf/utility/assertions.h"

namespace wf {

// Expression for expressing an unevaluated derivative operation.
// For example df(x)/dx is `Derivative(f(x), x)`. We use this to represent derivatives that can't
// be immediately evaluated.
class derivative {
 public:
  static constexpr std::string_view name_str = "Derivative";
  static constexpr bool is_leaf_node = false;

  derivative(scalar_expr differentiand, scalar_expr arg, const int order = 1)
      : children_{std::move(differentiand), std::move(arg)}, order_(order) {
    WF_ASSERT_GE(order_, 1);
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

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return derivative::create(operation(differentiand()), operation(argument()), order_);
  }

  constexpr const auto& children() const noexcept { return children_; }

  // Create a new derivative expression.
  static scalar_expr create(scalar_expr function, scalar_expr arg, int order);

 private:
  std::array<scalar_expr, 2> children_;
  int order_;
};

template <>
struct hash_struct<derivative> {
  std::size_t operator()(const derivative& func) const noexcept {
    return hash_args(static_cast<std::size_t>(func.order()), func.differentiand(), func.argument());
  }
};

template <>
struct is_identical_struct<derivative> {
  bool operator()(const derivative& a, const derivative& b) const {
    return a.order() == b.order() &&
           std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<derivative> {
  relative_order operator()(const derivative& a, const derivative& b) const {
    if (a.order() < b.order()) {
      return relative_order::less_than;
    } else if (a.order() > b.order()) {
      return relative_order::greater_than;
    }
    return wf::lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
