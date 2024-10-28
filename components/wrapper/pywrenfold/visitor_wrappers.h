// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>  // Required to pass std::vector.

#include "wf/any_expression.h"
#include "wf/substitute.h"
#include "wf/utility/algorithms.h"

// #include "wrapper_utils.h"

// This file is mostly a gross nuisance to handle the fact that we cannot pass std::variant
// directly to pybind methods (because it cannot be default initialized). So instead we have to
// write some adapter code so those methods can be wrapped - typically by passing a variant of
// const-pointers (which will at least produce correct type annotations). I'd like to be able
// to delete all of this.
namespace wf {

// Create a std::variant of const pointers to the types in `any_expression`:
template <typename T>
using add_const_ptr = std::add_pointer_t<std::add_const_t<T>>;
using any_expression_types = type_list_from_variant_t<any_expression>;
using any_const_ptr_to_expression =
    variant_from_type_list_t<type_list_map_t<add_const_ptr, any_expression_types>>;

// Accept variant of any_expression types const-ptr, and de-reference (copying in the process).
inline any_expression deref_any_ptr_to_expression(const any_const_ptr_to_expression& ptrs) {
  return std::visit(
      [](const auto ptr) -> any_expression {
        WF_ASSERT(ptr);
        return *ptr;
      },
      ptrs);
}

using scalar_ptr_pair = std::tuple<const scalar_expr*, const scalar_expr*>;
using boolean_ptr_pair = std::tuple<const boolean_expr*, const boolean_expr*>;
using scalar_ptr_or_boolean_ptr_pair = std::variant<scalar_ptr_pair, boolean_ptr_pair>;

// Wrapper for substitute() method that accepts a list of tuples.
template <typename X>
auto substitute_wrapper(const X& self, const std::vector<scalar_ptr_or_boolean_ptr_pair>& pairs) {
  // TODO: This conversion is a bit annoying, but it allows us to have correct type annotations.
  // We can probably avoid this by switching to nanobind.
  // See: https://github.com/wrenfold/wrenfold/issues/198
  const auto pairs_copied = transform_map<std::vector>(pairs, [](const auto& pair) {
    return std::visit(
        [](const auto& p) -> scalar_or_boolean_pair {
          return std::apply(
              [](auto... ptrs) {
                WF_ASSERT((static_cast<bool>(ptrs) && ...));
                return std::make_tuple(*ptrs...);
              },
              p);
        },
        pair);
  });

  if constexpr (std::is_same_v<X, any_const_ptr_to_expression>) {
    return substitute_any(deref_any_ptr_to_expression(self), pairs_copied);
  } else {
    return substitute(self, pairs_copied);
  }
}

// Wrapper for the single-replacement version of `substitute`.
template <typename X1, typename X2>
auto substitute_wrapper_single(const X1& self, const X2& target, const X2& replacement) {
  const std::array<scalar_or_boolean_pair, 1> pairs = {std::make_tuple(target, replacement)};
  if constexpr (std::is_same_v<X1, any_const_ptr_to_expression>) {
    return substitute_any(deref_any_ptr_to_expression(self), pairs);
  } else {
    return substitute(self, pairs);
  }
}

}  // namespace wf
