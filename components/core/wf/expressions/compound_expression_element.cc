// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/expressions/compound_expression_element.h"

#include "wf/expressions/custom_type_expressions.h"
#include "wf/expressions/external_function_invocation.h"
#include "wf/expressions/matrix.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {

struct get_nth_constructor_element {
  std::optional<scalar_expr> operator()(const scalar_expr& x, std::size_t& index) const noexcept {
    if (index == 0) {
      return x;
    } else {
      --index;
      return std::nullopt;
    }
  }

  std::optional<scalar_expr> operator()(const boolean_expr&, std::size_t& index) const {
    WF_ASSERT_NE(0, index,
                 "compound_expression_element cannot be used to retrieve boolean values yet.");
    --index;
    return std::nullopt;
  }

  std::optional<scalar_expr> operator()(const matrix_expr& m, std::size_t& index) const {
    if (index < m.size()) {
      const auto* mat = get_if<const matrix>(m);
      WF_ASSERT(mat != nullptr);
      return mat->children()[index];
    } else {
      index -= m.size();
      return std::nullopt;
    }
  }

  std::optional<scalar_expr> operator()(const compound_expr& c, std::size_t& index) const {
    if (const custom_type_construction* construct = get_if<const custom_type_construction>(c);
        construct != nullptr && construct->type().total_size() >= index) {
      for (const any_expression& child : construct->children()) {
        std::optional<scalar_expr> relevant_child = std::visit(
            [&](const auto& child_expr) { return operator()(child_expr, index); }, child);
        if (relevant_child.has_value()) {
          return relevant_child;
        }
      }
    }
    return std::nullopt;
  }
};

scalar_expr compound_expression_element::create(compound_expr provenance, const std::size_t index) {
  std::size_t counter = index;
  if (std::optional<scalar_expr> child = get_nth_constructor_element{}(provenance, counter);
      child.has_value()) {
    return *std::move(child);
  }
  return scalar_expr(std::in_place_type_t<compound_expression_element>{}, std::move(provenance),
                     index);
}

}  // namespace wf
