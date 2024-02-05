// Copyright 2023 Gareth Cross
#pragma once
#include <algorithm>
#include <variant>

#include <pybind11/pybind11.h>

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"

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
    klass.def("is_identical_to", &are_identical<T>, py::arg("other"),
              py::doc("Check for strict equality."));
    klass.def(
        "__eq__", &are_identical<T>, py::is_operator(), py::arg("other"),
        py::doc("Check for strict equality. This is not the same as mathematical equivalence."));
  }
  return klass;
}

// Iterate over a container and transform every element into `Expr`.
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
                   return py::cast<Expr>(handle);
                 });
  return count;
}

// Try converting `x` to an int or float, otherwise just return Expr.
inline std::variant<std::int64_t, double, Expr> try_convert_to_numeric(const Expr& x) {
  if (const float_constant* f = cast_ptr<const float_constant>(x); f != nullptr) {
    return f->get_value();
  } else if (const integer_constant* i = cast_ptr<const integer_constant>(x); i != nullptr) {
    return i->get_value();
  } else {
    return x;
  }
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
