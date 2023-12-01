// Copyright 2023 Gareth Cross
#pragma once

#include "wf/functions.h"
#include "wf/hashing.h"
#include "wf/visitor_impl.h"

namespace math {

// Cast a boolean value to an integer numerical value.
class CastBool {
 public:
  static constexpr std::string_view NameStr = "CastBool";
  static constexpr bool IsLeafNode = false;

  explicit CastBool(Expr arg) noexcept(std::is_nothrow_move_constructible_v<Expr>)
      : arg_{std::move(arg)} {}

  constexpr const Expr& arg() const noexcept { return arg_[0]; }

  // Iterators
  constexpr auto begin() const noexcept { return arg_.begin(); }
  constexpr auto end() const noexcept { return arg_.end(); }

  // Function type and argument must match.
  bool is_identical_to(const CastBool& other) const {
    return arg_[0].is_identical_to(other.arg_[0]);
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void for_each(Operation&& operation) const {
    std::for_each(begin(), end(), std::forward<Operation>(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr map_children(Operation&& operation) const {
    Expr transformed = operation(arg_.front());
    return cast_int_from_bool(transformed);
  }

 private:
  std::array<Expr, 1> arg_;
};

template <>
struct hash_struct<CastBool> {
  std::size_t operator()(const CastBool& cast) const { return hash(cast.arg()); }
};

}  // namespace math
