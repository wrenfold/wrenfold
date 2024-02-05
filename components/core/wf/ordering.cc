// Copyright 2023 Gareth Cross
#include "wf/ordering.h"

#include "wf/expression.h"
#include "wf/expressions/all_expressions.h"
#include "wf/visit.h"

namespace wf {

// Visitor that can be used to sort expressions by determining their relative order.
struct order_visitor {
  template <typename T>
  relative_order operator()(const T& a, const T& b) const {
    if constexpr (implements_order_struct_v<T>) {
      return order_struct<T>{}(a, b);
    } else {
      // TODO: Move all definitions to be beside their respective types.
      return compare(a, b);
    }
  }

  // This visitor applies to any two members of `OrderOfTypes` that are _not_ the same type.
  template <typename Numeric,
            typename = enable_if_contains_type_t<Numeric, float_constant, integer_constant,
                                                 rational_constant, symbolic_constant>>
  constexpr relative_order compare(const Numeric& a, const Numeric& b) const noexcept {
    if (a < b) {
      return relative_order::less_than;
    } else if (b < a) {
      return relative_order::greater_than;
    }
    return relative_order::equal;
  }

  relative_order compare(const cast_bool& a, const cast_bool& b) const {
    return order_struct<scalar_expr>{}(a.arg(), b.arg());
  }

  relative_order compare(const conditional& a, const conditional& b) const {
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                     order_struct<scalar_expr>{});
  }

  constexpr relative_order compare(const complex_infinity&,
                                   const complex_infinity&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const addition& a, const addition& b) const {
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                     order_struct<scalar_expr>{});
  }

  relative_order compare(const derivative& a, const derivative& b) const {
    if (a.order() < b.order()) {
      return relative_order::less_than;
    } else if (a.order() > b.order()) {
      return relative_order::greater_than;
    }
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                     order_struct<scalar_expr>{});
  }

  relative_order compare(const multiplication& a, const multiplication& b) const {
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                     order_struct<scalar_expr>{});
  }

  relative_order compare(const power& a, const power& b) const {
    return wf::lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                     order_struct<scalar_expr>{});
  }

  relative_order compare(const function& a, const function& b) const {
    // First compare by name:
    const int name_comp = a.function_name().compare(b.function_name());
    if (name_comp > 0) {
      return relative_order::greater_than;
    } else if (name_comp < 0) {
      return relative_order::less_than;
    }
    return lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                 order_struct<scalar_expr>{});
  }

  relative_order compare(const relational& a, const relational& b) const {
    if (a.operation() < b.operation()) {
      return relative_order::less_than;
    } else if (a.operation() > b.operation()) {
      return relative_order::greater_than;
    }
    return lexicographical_order(a.begin(), a.end(), b.begin(), b.end(),
                                 order_struct<scalar_expr>{});
  }

  constexpr relative_order compare(const undefined&, const undefined&) const noexcept {
    return relative_order::equal;
  }

  relative_order compare(const variable& a, const variable& b) const {
    if (a.identifier() < b.identifier()) {
      return relative_order::less_than;
    } else if (a.is_identical_to(b)) {
      return relative_order::equal;
    }
    return relative_order::greater_than;
  }
};

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
                complex_infinity, variable, multiplication, addition, power, function, relational,
                conditional, cast_bool, compound_expression_element, derivative, undefined>;
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
    const auto& b_typed = cast_unchecked<const Ta>(b);
    return order_visitor{}(a_typed, b_typed);
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
    return order_visitor{}(a_typed, cast_unchecked<const Ta>(b));
  });
}

}  // namespace wf
