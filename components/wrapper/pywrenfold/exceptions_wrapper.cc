// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/nanobind.h>

#include "wf/utility/error_types.h"

namespace py = nanobind;
using namespace py::literals;

namespace wf {

void wrap_exceptions(py::module_& m) {
  py::exception<arithmetic_error>(m, "ArithmeticError").doc() =
      "Thrown when invalid arithmetic is attempted.";
  py::exception<assertion_error>(m, "AssertionError").doc() =
      "Thrown for internal errors. Report these on `GitHub "
      "<https://github.com/wrenfold/wrenfold/issues>`_.";
  py::exception<dimension_error>(m, "DimensionError").doc() =
      "Thrown when matrix operations encounter invalid dimensions.";
  py::exception<domain_error>(m, "DomainError").doc() =
      "Thrown when an argument exceeds acceptable numeric bounds.";
  py::exception<invalid_argument_error>(m, "InvalidArgumentError").doc() =
      "Thrown for invalid argument values.";
  py::exception<type_error>(m, "TypeError").doc() =
      "Thrown when an unsupported type is encountered.";
}

}  // namespace wf
