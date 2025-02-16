// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "docs/expressions_wrapper.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// TODO: Wrap all the underlying expressions so they can be visited in python.
// Initially we wrap just the `Variable` type.
void wrap_expressions(pybind11::module&) {}

}  // namespace wf
