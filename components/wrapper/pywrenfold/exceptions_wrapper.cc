// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/utility/error_types.h"

#include "pywrenfold/pybind_imports.h"

using namespace py::literals;

namespace wf {

void wrap_exceptions(py::module_& m) {
  py::register_exception<arithmetic_error>(m, "ArithmeticError").doc() =
      "Thrown when invalid arithmetic is attempted.";
  py::register_exception<assertion_error>(m, "AssertionError").doc() =
      "Thrown for internal errors.";
  py::register_exception<dimension_error>(m, "DimensionError").doc() =
      "Thrown when matrix operations encounter invalid dimensions.";
  py::register_exception<domain_error>(m, "DomainError");
  py::register_exception<invalid_argument_error>(m, "InvalidArgumentError").doc() =
      "Thrown for invalid argument values.";
  py::register_exception<type_error>(m, "TypeError").doc() =
      "Thrown when an unsupported type is encountered.";
}

}  // namespace wf
