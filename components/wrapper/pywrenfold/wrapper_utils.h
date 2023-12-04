// Copyright 2023 Gareth Cross
#pragma once
#include <variant>
#include <vector>

#include <pybind11/pybind11.h>

#include "wf/expression.h"
#include "wf/expressions/numeric_expressions.h"

// Shared utilities for writing wrappers.
namespace wf {
namespace py = pybind11;

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
  if (const float_constant* f = cast_ptr<float_constant>(x); f != nullptr) {
    return f->get_value();
  } else if (const integer_constant* i = cast_ptr<integer_constant>(x); i != nullptr) {
    return i->get_value();
  } else {
    return x;
  }
}

}  // namespace wf
