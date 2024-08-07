// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"

namespace wf {

// A set of parentheses about a scalar-valued expression that stops further simplification.
class unevaluated {
 public:
  static constexpr std::string_view name_str = "Unevaluated";
  static constexpr bool is_leaf_node = false;

  explicit unevaluated(scalar_expr contents) noexcept : contents_(std::move(contents)) {}

  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return create(operation(contents_));
  }

  constexpr absl::Span<const scalar_expr> children() const noexcept {
    return {std::addressof(contents_), static_cast<std::size_t>(1)};
  }

  // Create a new unevaluated expression.
  static scalar_expr create(scalar_expr contents);

  constexpr const scalar_expr& contents() const noexcept { return contents_; }

  // Support iteration.
  constexpr auto begin() const noexcept { return std::addressof(contents_); }
  constexpr auto end() const noexcept { return begin() + 1; }

  static constexpr std::size_t size() noexcept { return 1; }

 protected:
  scalar_expr contents_;
};

template <>
struct hash_struct<unevaluated> {
  std::size_t operator()(const unevaluated& c) const noexcept { return hash(c.contents()); }
};

template <>
struct is_identical_struct<unevaluated> {
  bool operator()(const unevaluated& a, const unevaluated& b) const {
    return are_identical(a.contents(), b.contents());
  }
};

template <>
struct order_struct<unevaluated> {
  relative_order operator()(const unevaluated& a, const unevaluated& b) const {
    return wf::order_by(a.contents(), b.contents());
  }
};

}  // namespace wf
