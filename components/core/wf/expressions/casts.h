// Copyright 2023 Gareth Cross
#pragma once

#include "wf/functions.h"
#include "wf/hashing.h"

namespace wf {

// Cast a boolean value to an integer numerical value.
class cast_bool {
 public:
  static constexpr std::string_view name_str = "CastBool";
  static constexpr bool is_leaf_node = false;

  explicit cast_bool(scalar_expr arg) noexcept(std::is_nothrow_move_constructible_v<scalar_expr>)
      : arg_{std::move(arg)} {}

  constexpr const scalar_expr& arg() const noexcept { return arg_[0]; }

  // Iterators
  constexpr auto begin() const noexcept { return arg_.begin(); }
  constexpr auto end() const noexcept { return arg_.end(); }

  // Function type and argument must match.
  bool is_identical_to(const cast_bool& other) const {
    return arg_[0].is_identical_to(other.arg_[0]);
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  scalar_expr map_children(Operation&& operation) const {
    return cast_int_from_bool(operation(arg_.front()));
  }

 private:
  std::array<scalar_expr, 1> arg_;
};

template <>
struct hash_struct<cast_bool> {
  std::size_t operator()(const cast_bool& cast) const { return hash(cast.arg()); }
};

}  // namespace wf
