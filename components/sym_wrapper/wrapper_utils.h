// Copyright 2023 Gareth Cross
#pragma once
#include <pybind11/pybind11.h>
#include <variant>
#include <vector>

#include "expression.h"
#include "expressions/numeric_expressions.h"

// Shared utilities for writing wrappers.
namespace math {
namespace py = pybind11;

// Iterate over a container and transform every element into `Expr`.
template <typename Container, typename Output>
std::size_t TransformIntoExpr(const Container& inputs, Output& output) {
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
inline std::variant<std::int64_t, double, Expr> TryConvertToNumeric(const Expr& x) {
  if (const Float* f = CastPtr<Float>(x); f != nullptr) {
    return f->get_value();
  } else if (const Integer* i = CastPtr<Integer>(x); i != nullptr) {
    return i->get_value();
  } else {
    return x;
  }
}

}  // namespace math
