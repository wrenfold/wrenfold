// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/functions.h"
#include "wf/utility/hashing.h"

namespace wf {

// Cast a boolean value to an integer numerical value.
class iverson_bracket {
 public:
  static constexpr std::string_view name_str = "IversonBracket";
  static constexpr bool is_leaf_node = false;

  explicit iverson_bracket(boolean_expr arg) noexcept : arg_{std::move(arg)} {}

  constexpr const boolean_expr& arg() const noexcept { return arg_[0]; }

  // Iterators
  constexpr auto begin() const noexcept { return arg_.begin(); }
  constexpr auto end() const noexcept { return arg_.end(); }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return iverson(operation(arg_.front()));
  }

  constexpr const auto& children() const noexcept { return arg_; }

 private:
  std::array<boolean_expr, 1> arg_;
};

template <>
struct hash_struct<iverson_bracket> {
  std::size_t operator()(const iverson_bracket& cast) const { return hash(cast.arg()); }
};

template <>
struct is_identical_struct<iverson_bracket> {
  bool operator()(const iverson_bracket& a, const iverson_bracket& b) const {
    return are_identical(a.arg(), b.arg());
  }
};

template <>
struct order_struct<iverson_bracket> {
  relative_order operator()(const iverson_bracket& a, const iverson_bracket& b) const {
    return order_by(a.arg(), b.arg());
  }
};

}  // namespace wf
