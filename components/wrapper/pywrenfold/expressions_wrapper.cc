// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/plain_formatter.h"
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/expressions/variable.h"

#include "docs/expressions_wrapper.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// TODO: Wrap all the underlying expressions so they can be visited in python.
// Initially we wrap just the `Variable` type.
void wrap_expressions(pybind11::module& m) {
  wrap_class<variable>(m, "Variable")
      .def(py::init<std::string, number_set>(), py::arg("name"),
           py::arg("number_set") = number_set::unknown, docstrings::variable_constructor.data())
      .def_property_readonly("name", &variable::name, "Name of the variable.")
      .def_property_readonly("set", &variable::set, "Numeric set the variable belongs to.")
      .def(
          "to_expression",
          [](const variable& self) {
            // TODO: This should not be a copy. Ideally `self` would already be the shared ptr.
            return scalar_expr(self);
          },
          "Convert the variable back to a :class:`wrenfold.sym.Expr`.")
      .def("__repr__", [](const variable& self) { return self.name(); })
      .doc() = "Concrete expression for a symbolic variable.";
}

}  // namespace wf
