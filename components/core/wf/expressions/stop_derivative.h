// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <span>
#include <string_view>

#include "wf/expression.h"

namespace wf {

// Expression that stops propagation of derivatives.
// Taking the derivative of `stop_derivative` is always zero.
class stop_derivative {
 public:
  static constexpr std::string_view name_str = "StopDerivative";
  static constexpr bool is_leaf_node = false;

  explicit stop_derivative(scalar_expr arg) noexcept : arg_(std::move(arg)) {}

  constexpr auto begin() const noexcept { return std::addressof(arg_); }
  constexpr auto end() const noexcept { return begin() + 1; }

  constexpr const auto& arg() const noexcept { return arg_; }

  // Get a span over the single child.
  constexpr auto children() const noexcept {
    return std::span<const scalar_expr, 1>{begin(), end()};
  }

  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return create(operation(arg_));
  }

  // Wrap `arg` in `null_derivative`.
  // If the argument is already a `null_derivative`, we don't add another.
  static scalar_expr create(scalar_expr arg);

 private:
  scalar_expr arg_;
};

template <>
struct hash_struct<stop_derivative> {
  std::size_t operator()(const stop_derivative& n) const noexcept { return hash(n.arg()); }
};

template <>
struct is_identical_struct<stop_derivative> {
  bool operator()(const stop_derivative& a, const stop_derivative& b) const {
    return are_identical(a.arg(), b.arg());
  }
};

template <>
struct order_struct<stop_derivative> {
  relative_order operator()(const stop_derivative& a, const stop_derivative& b) const {
    return order_by(a.arg(), b.arg());
  }
};

}  // namespace wf
