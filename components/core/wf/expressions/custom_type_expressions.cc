// Copyright 2024 Gareth Cross
#include "wf/expressions/custom_type_expressions.h"

#include "wf/visit.h"

namespace wf {

custom_type_construction::custom_type_construction(custom_type type, std::vector<scalar_expr> args)
    : type_(std::move(type)), args_(std::move(args)) {
  WF_ASSERT_EQUAL(
      type_.total_size(), args_.size(),
      "Mismatch between size of custom type `{}` ({}) and the number of provided args ({}).",
      type_.name(), type_.total_size(), args_.size());
}

// Check if the underlying type of the compound expression is a custom type.
// Return a pointer to the custom type if it is.
static maybe_null<const custom_type*> maybe_get_custom_type(const compound_expr& expr) {
  return visit(
      expr, make_overloaded(
                [](const external_function_invocation& invocation) -> const custom_type* {
                  return std::get_if<custom_type>(&invocation.function().return_type());
                },
                [](const custom_type_argument& arg) -> const custom_type* { return &arg.type(); },
                [](const custom_type_construction& construct) -> const custom_type* {
                  return &construct.type();
                }));
}

// Check if `args` reduces to a list of `compound_expression_elements` that together form an
// existing compound expression. We need to check that the underlying type matches, and that we have
// the right number of elements in the appropriate order.
static std::optional<compound_expr> maybe_get_existing_compound_expr(
    const custom_type& type, const custom_type_construction::container_type& args) {
  if (args.empty()) {
    return std::nullopt;
  }

  // Get the first element, which we use to get the inner compound expression type.
  const compound_expression_element* first_element =
      cast_ptr<const compound_expression_element>(args[0]);
  if (!first_element || first_element->index() != 0) {
    return std::nullopt;
  }

  // If the inner type doesn't match, no point checking the rest of the expressions.
  const compound_expr& provenance = first_element->provenance();
  if (const maybe_null<const custom_type*> inner_type = maybe_get_custom_type(provenance);
      !inner_type.has_value() || !are_identical(*inner_type, type)) {
    return std::nullopt;
  }

  // Now check if (in aggregate) the vector of expressions is just a copy of `provenance`.
  for (std::size_t i = 1; i < args.size(); ++i) {
    if (const auto* element = cast_ptr<const compound_expression_element>(args[i]);
        element == nullptr || element->index() != i ||
        element->provenance().hash() != provenance.hash() ||
        !are_identical(element->provenance(), provenance)) {
      return std::nullopt;
    }
  }

  // All the inner expressions are just elements of `provenance`, in order.
  return provenance;
}

compound_expr custom_type_construction::create(custom_type type, container_type args) {
  if (auto existing = maybe_get_existing_compound_expr(type, args); existing.has_value()) {
    return *std::move(existing);
  }
  return compound_expr{std::in_place_type_t<custom_type_construction>{}, std::move(type),
                       std::move(args)};
}

}  // namespace wf
