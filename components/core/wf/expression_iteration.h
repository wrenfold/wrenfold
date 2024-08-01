// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/any_expression.h"
#include "wf/expression_visitor.h"
#include "wf/utility/traits.h"

namespace wf {
namespace detail {
template <typename X, typename F>
void maybe_visit_any_expression(const X& expr, F&& f) {
  if constexpr (std::is_same_v<X, any_expression>) {
    std::visit(f, expr);
  } else {
    f(expr);
  }
}
}  // namespace detail

template <typename T, typename F>
void for_each_child(const T& concrete_expr, F&& func) {
  if constexpr (!T::is_leaf_node) {
    const auto& children = concrete_expr.children();
    using container_type = std::decay_t<decltype(children)>;
    static_assert(is_iterable_v<container_type> || is_tuple_v<container_type>,
                  "Children must be an iterable container or a tuple");
    if constexpr (is_iterable_v<container_type>) {
      for (const auto& child : children) {
        detail::maybe_visit_any_expression(child, std::forward<F>(func));
      }
    } else if constexpr (is_tuple_v<container_type>) {
      std::apply(
          [&func](const auto&... child) {
            (detail::maybe_visit_any_expression(child, std::forward<F>(func)), ...);
          },
          children);
    }
  }
}

}  // namespace wf
