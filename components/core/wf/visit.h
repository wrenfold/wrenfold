// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression.h"
#include "wf/template_utils.h"

// All expression definitions need to be available to static_cast.
#include "wf/expressions/all_expressions.h"

namespace wf {

// Accepts a visitor struct or lambda and applies it to the provided expression.
// The return type will be deduced from the visitor itself.
template <typename F>
auto visit(const Expr& expr, F&& visitor) noexcept(
    is_nothrow_invocable_visitor_v<decltype(visitor), Expr::storage_type::types>) {
  return expr.impl().visit(std::forward<F>(visitor));
}

// Version of `visit` that accepts visitors that can re-use the input expression.
// Methods on the visitor can optionally be of the form operator()(x, expr) where `x`
// is the underlying expression type and `expr` is the abstract `Expr`. This allows
// optionally re-using the expression to avoid allocation.
template <typename VisitorType>
auto visit_with_expr(const Expr& expr, VisitorType&& visitor) {
  return visit(expr, [&expr, &visitor](const auto& x) {
    using T = std::decay_t<decltype(x)>;

    // Make sure this is not ambiguous:
    static_assert(has_binary_call_operator_v<VisitorType, const T&, const Expr&> !=
                      has_call_operator_v<VisitorType, const T&>,
                  "Visitor must support either unary or binary operator(), but not both.");

    if constexpr (has_binary_call_operator_v<VisitorType, const T, const Expr>) {
      return visitor(x, expr);
    } else {
      return visitor(x);
    }
  });
}

// Visit two expressions with a struct that accepts two concrete types in its operator()(...)
// signature. The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto visit_binary(const Expr& u, const Expr& v, VisitorType&& handler) {
  return visit(u, [&handler, &v](const auto& typed_u) {
    return visit(v, [&handler, &typed_u](const auto& typed_v) {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(!std::is_const_v<decltype(handler)>);
      static_assert(has_binary_call_operator_v<std::decay_t<VisitorType>, const TypeU, const TypeV>,
                    "Binary visitor fails to implement a required operator() method.");
      return handler(typed_u, typed_v);
    });
  });
}

}  // namespace wf
