// Copyright 2023 Gareth Cross
#pragma once
#include "wf/expression_variant.h"
#include "wf/template_utils.h"

// All expression definitions need to be available to static_cast.
#include "wf/expressions/all_expressions.h"

namespace wf {

// Accepts a visitor struct or lambda and applies it to the provided expression.
// The return type will be deduced from the visitor itself.
template <typename D, typename M, typename F>
auto visit(const expression_base<D, M>& expr, F&& visitor) {
  return expr.impl().visit([&visitor, &expr](const auto& contents) {
    using T = std::decay_t<decltype(contents)>;

    // Make sure this is not ambiguous:
    static_assert(is_invocable_v<F, const T&, const D&> || is_invocable_v<F, const T&>,
                  "Visitor must support at least one version of operator().");
    static_assert(is_invocable_v<F, const T&, const D&> != is_invocable_v<F, const T&>,
                  "Visitor must support either unary or binary operator(), but not both.");

    if constexpr (is_invocable_v<F, const T&, const D&>) {
      return visitor(contents, expr.as_derived());
    } else {
      return visitor(contents);
    }
  });
}

// Visit a variant of different `expression_base` types.
// We use enable_if to disallow implicit conversion to the variant type.
template <typename Var, typename F, typename = std::enable_if_t<is_variant_v<Var>>>
auto visit(const Var& variant, F&& visitor) {
  return std::visit(
      [&visitor](const auto& x) {
        using T = std::decay_t<decltype(x)>;
        if constexpr (inherits_expression_base_v<T>) {
          return visit(x, std::forward<F>(visitor));
        } else {
          // TODO: Fallback path for matrix_expr - maybe temporary?
          return visitor(x);
        }
      },
      variant);
}

// Visit two expressions with an invocable that accepts two concrete types in its operator()(...)
// signature.
template <typename U, typename V, typename VisitorType>
auto visit_binary(const U& u, const V& v, VisitorType&& handler) {
  return visit(u, [&handler, &v](const auto& typed_u) {
    return visit(v, [&handler, &typed_u](const auto& typed_v) {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(is_invocable_v<VisitorType, const TypeU&, const TypeV&>,
                    "Binary visitor fails to implement a required operator() method.");
      return handler(typed_u, typed_v);
    });
  });
}

// TODO: Document.
template <typename F>
compound_expr map_compound_expressions(const compound_expr& expr, F&& f) {
  return visit(expr, make_overloaded(
                         [&](const external_function_invocation& invocation) {
                           return invocation.map_children([&](const any_expression& arg) {
                             // TODO: Make `matrix_expr` derived from `expression_base`, and call
                             //  visit(...) here.
                             return overloaded_visit(
                                 arg, [&](const Expr& x) -> any_expression { return f(x); },
                                 [&](const matrix_expr& x) -> any_expression {
                                   matrix m = x.as_matrix().map_children(std::forward<F>(f));
                                   return matrix_expr(std::move(m));
                                 },
                                 [&](const compound_expr& x) -> any_expression { return f(x); });
                           });
                         },
                         [&](const custom_type_argument&) { return expr; },
                         [&](const custom_type_construction& construct) {
                           return construct.map_children(std::forward<F>(f));
                         }));
}

}  // namespace wf
