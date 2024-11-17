// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression_variant.h"
#include "wf/utility/visit_switch.h"

// All expression definitions need to be available to static_cast.
#include "wf/expressions/all_expressions.h"

namespace wf {

// Visit the specified expression with `visitor`. The visitor will be passed a
// const reference to the concrete underlying contents of the abstract expression.
//
// The signature of `visitor` can take one of two forms:
//
//  (1) ret operator()(const T&);
//  (2) ret operator()(const T&, const D&);
//
// Where `T` is the concrete inner type of the expression, and `D` is the outer
// abstract expression type.
//
// The return type will be deduced from the visitor itself.
template <typename D, typename M, typename F>
auto visit(const expression_base<D, M>& expr, F&& visitor) {
  using types = typename expression_base<D, M>::types;

  return detail::visit_switch<type_list_size_v<types>>(
      expr.type_index(), [&visitor, &expr](const auto integral_constant) {
        constexpr std::size_t idx = integral_constant();
        using T = type_list_element_t<idx, types>;

        // Make sure this is not ambiguous:
        static_assert(is_invocable_v<F, const T&, const D&> || is_invocable_v<F, const T&>,
                      "Visitor must support at least one version of operator().");
        static_assert(is_invocable_v<F, const T&, const D&> != is_invocable_v<F, const T&>,
                      "Visitor must support either unary or binary operator(), but not both.");

        if constexpr (is_invocable_v<F, const T&, const D&>) {
          return visitor(detail::cast_to_index<idx>(expr.impl()), expr.as_derived());
        } else {
          return visitor(detail::cast_to_index<idx>(expr.impl()));
        }
      });
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

}  // namespace wf
