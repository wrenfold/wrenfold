// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <algorithm>
#include <variant>

#include <pybind11/pybind11.h>

#include "wf/expression.h"
#include "wf/substitute.h"
#include "wf/utility/algorithms.h"

// Shared utilities for writing wrappers.
namespace wf {
namespace py = pybind11;

// Wrap a class, and add certain members automatically.
template <typename T>
py::class_<T> wrap_class(py::module_& m, const std::string_view name) {
  py::class_<T> klass(m, name.data());
  if constexpr (is_invocable_v<hash_struct<T>, const T&>) {
    klass.def("__hash__", &hash<T>, py::doc("Compute hash."));
  }
  if constexpr (is_invocable_v<is_identical_struct<T>, const T&, const T&>) {
    klass.def(
        "is_identical_to", &are_identical<T>, py::arg("other"),
        py::doc("Check for strict equality. This is not the same as mathematical equivalence."));
    klass.def(
        "__eq__", &are_identical<T>, py::is_operator(), py::arg("other"),
        py::doc("Check for strict equality. This is not the same as mathematical equivalence."));
  }
  return klass;
}

// Iterate over a container and transform every element into `scalar_expr`.
template <typename Container, typename Output>
std::size_t cast_to_expr(const Container& inputs, Output& output) {
  // len_hint will get the length if possible, otherwise return 0.
  if constexpr (std::is_base_of_v<py::handle, std::decay_t<Container>>) {
    output.reserve(output.size() + py::len_hint(inputs));
  } else {
    output.reserve(output.size() + inputs.size());
  }
  // Count so we only traverse the input iterators once (to handle generators).
  std::size_t count = 0;
  std::transform(inputs.begin(), inputs.end(), std::back_inserter(output),
                 [&](const py::handle& handle) {
                   ++count;
                   return py::cast<scalar_expr>(handle);
                 });
  return count;
}

// We can't pass std::variant<...> directly (because it is not default initializable), but we can
// pass a variant of pointers. We use these so we can wrap substitute() and still have some good
// type annotations in the generated stubs. (See note below about nanobind...)
using scalar_ptr_pair = std::tuple<const scalar_expr*, const scalar_expr*>;
using boolean_ptr_pair = std::tuple<const boolean_expr*, const boolean_expr*>;
using scalar_ptr_or_boolean_ptr_pair = std::variant<scalar_ptr_pair, boolean_ptr_pair>;

// Wrapper for substitute() method that accepts a list of tuples.
template <typename X>
X substitute_wrapper(const X& self, const std::vector<scalar_ptr_or_boolean_ptr_pair>& pairs) {
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
  return substitute(self, pairs_copied);
}

// Wrapper for the single-replacement version of `substitute`.
template <typename X1, typename X2>
X1 substitute_wrapper_single(const X1& self, const X2& target, const X2& replacement) {
  const std::array<scalar_or_boolean_pair, 1> pairs = {std::make_tuple(target, replacement)};
  return substitute(self, pairs);
}

namespace detail {

template <typename... Ts>
std::array<std::string, sizeof...(Ts)> get_py_type_names(type_list<Ts...>) {
  return {static_cast<std::string>(py::repr(py::type::of<Ts>()))...};
}

template <typename V, std::size_t I>
V variant_from_pyobject(const py::handle& object) {
  using list = type_list_from_variant_t<V>;
  using type = type_list_element_t<I, list>;
  if (py::isinstance<type>(object)) {
    return py::cast<type>(object);
  } else if constexpr (I + 1 < type_list_size_v<list>) {
    return variant_from_pyobject<V, I + 1>(object);
  } else {
    static const auto types = detail::get_py_type_names(list{});
    throw type_error("Object of type `{}` is not one of the permitted types: {}",
                     static_cast<std::string>(py::repr(py::type::of(object))),
                     fmt::join(types, ", "));
  }
}
}  // namespace detail

// Try to cast py::object to the specified variant.
template <typename V>
V variant_from_pyobject(const py::handle& object) {
  return detail::variant_from_pyobject<V, 0>(object);
}

}  // namespace wf
