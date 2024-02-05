// Copyright 2023 Gareth Cross
#include "wf/expressions/compound_expression_element.h"

#include "wf/expressions/custom_type_expressions.h"
#include "wf/expressions/external_function_invocation.h"

namespace wf {

scalar_expr compound_expression_element::create(compound_expr provenance, const std::size_t index) {
  if (const custom_type_construction* construct =
          cast_ptr<const custom_type_construction>(provenance);
      construct != nullptr) {
    return construct->at(index);
  }
  return scalar_expr(std::in_place_type_t<compound_expression_element>{}, std::move(provenance),
                     index);
}

}  // namespace wf
