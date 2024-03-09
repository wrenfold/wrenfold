// Copyright 2023 Gareth Cross
#include "wf/ordering.h"

#include "wf/expression.h"
#include "wf/expression_visitor.h"
#include "wf/expressions/all_expressions.h"

namespace wf {

template <typename... AllTypes, typename... OrderedTypes>
static constexpr auto get_type_order_indices(type_list<AllTypes...>,
                                             type_list<OrderedTypes...>) noexcept {
  using all_types_list = type_list<AllTypes...>;
  using ordered_list = type_list<OrderedTypes...>;

  // The lists should contain the same elements, just in a different order:
  static_assert(sizeof...(AllTypes) == sizeof...(OrderedTypes));
  static_assert(std::conjunction_v<type_list_contains<OrderedTypes, all_types_list>...>,
                "A type in OrderedTypes is not present in AllTypes.");
  static_assert(std::conjunction_v<type_list_contains<AllTypes, ordered_list>...>,
                "A type in AllTypes is not present in OrderedTypes.");

  // Return array mapping from [index in expression_variant] --> [index in ordered list].
  return std::array<uint16_t, sizeof...(AllTypes)>{
      static_cast<uint16_t>(type_list_index_v<AllTypes, ordered_list>)...};
}

relative_order order_struct<scalar_expr>::operator()(const scalar_expr& a,
                                                     const scalar_expr& b) const {
  using order_of_types =
      type_list<float_constant, integer_constant, rational_constant, symbolic_constant,
                complex_infinity, imaginary_unit, variable, multiplication, addition, power,
                function, conditional, iverson_bracket, compound_expression_element, derivative,
                undefined>;
  static constexpr auto order =
      get_type_order_indices(scalar_expr::storage_type::types{}, order_of_types{});

  const auto index_a = order[a.type_index()];
  const auto index_b = order[b.type_index()];
  if (index_a < index_b) {
    return relative_order::less_than;
  } else if (index_a > index_b) {
    return relative_order::greater_than;
  }

  // Otherwise we have the same type:
  return visit(a, [&b](const auto& a_typed) -> relative_order {
    using Ta = std::decay_t<decltype(a_typed)>;
    static_assert(is_orderable_v<Ta>, "Type does not implement order_struct.");

    const auto& b_typed = cast_unchecked<const Ta>(b);
    return order_struct<Ta>{}(a_typed, b_typed);
  });
}

relative_order order_struct<compound_expr>::operator()(const compound_expr& a,
                                                       const compound_expr& b) const {
  if (a.type_index() < b.type_index()) {
    return relative_order::less_than;
  } else if (a.type_index() > b.type_index()) {
    return relative_order::greater_than;
  }
  return visit(a, [&b](const auto& a_typed) -> relative_order {
    using Ta = std::decay_t<decltype(a_typed)>;
    static_assert(is_orderable_v<Ta>, "Type does not implement order_struct.");
    return order_struct<Ta>{}(a_typed, cast_unchecked<const Ta>(b));
  });
}

relative_order order_struct<boolean_expr>::operator()(const boolean_expr& a,
                                                      const boolean_expr& b) const {
  if (a.type_index() < b.type_index()) {
    return relative_order::less_than;
  } else if (a.type_index() > b.type_index()) {
    return relative_order::greater_than;
  }
  return visit(a, [&b](const auto& a_typed) -> relative_order {
    using Ta = std::decay_t<decltype(a_typed)>;
    static_assert(is_orderable_v<Ta>, "Type does not implement order_struct.");
    return order_struct<Ta>{}(a_typed, cast_unchecked<const Ta>(b));
  });
}

}  // namespace wf
