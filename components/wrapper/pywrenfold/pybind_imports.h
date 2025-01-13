// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <type_traits>

#include "wf/expression.h"

#include <pybind11/complex.h>     // std::complex
#include <pybind11/functional.h>  // std::function
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>  // std::vector, std::variant, std::optional

namespace py = pybind11;

namespace pybind11::detail {

// Custom type caster that converts scalar_expr to a concrete expression type.
template <typename T>
struct type_caster<T, std::enable_if_t<std::is_same_v<std::remove_reference_t<T>, wf::scalar_expr>>>
    : public type_caster_base<T> {
  using base = type_caster_base<wf::scalar_expr>;
 public:
};

}  // namespace pybind11::detail

namespace wf {}  // namespace wf
