// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"

namespace wf {

// An incomplete or deferred substitution: subs(f(x), x, y) --> f(y)
class substitution {
 public:
  static constexpr std::string_view name_str = "Substitution";
  static constexpr bool is_leaf_node = false;

  substitution(scalar_expr input, scalar_expr target, scalar_expr replacement) noexcept
      : children_{std::move(input), std::move(target), std::move(replacement)} {}

  // The expression `f(x)` that we modify.
  constexpr const scalar_expr& input() const noexcept { return children_[0]; }

  // Target expression `x` that is replaced.
  constexpr const scalar_expr& target() const noexcept { return children_[1]; }

  // The substitution for `x` that we insert.
  constexpr const scalar_expr& replacement() const noexcept { return children_[2]; }

  // Access children as iterator.
  constexpr auto begin() const noexcept { return children_.begin(); }
  constexpr auto end() const noexcept { return children_.end(); }

  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return substitution::create(operation(input()), operation(target()), operation(replacement()));
  }

  constexpr const auto& children() const noexcept { return children_; }

  static scalar_expr create(scalar_expr input, scalar_expr target, scalar_expr replacement);

 private:
  std::array<scalar_expr, 3> children_;
};

template <>
struct hash_struct<substitution> {
  std::size_t operator()(const substitution& func) const noexcept { return hash_all(0, func); }
};

template <>
struct is_identical_struct<substitution> {
  bool operator()(const substitution& a, const substitution& b) const {
    return std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<scalar_expr>{});
  }
};

template <>
struct order_struct<substitution> {
  relative_order operator()(const substitution& a, const substitution& b) const {
    return wf::lexicographical_order(a, b, order_struct<scalar_expr>{});
  }
};

}  // namespace wf
