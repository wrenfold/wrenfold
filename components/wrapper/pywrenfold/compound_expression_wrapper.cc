// Copyright 2024 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>

#include "wf/code_generation/types.h"  //  required for definition of custom_type
#include "wf/compound_expression.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

void wrap_compound_expression(py::module_& m) {
  py::class_<compound_expr>(m, "CompoundExpr")
      .def("is_identical_to", &are_identical<compound_expr>,
           py::doc("Check if two compound expressions are strictly identical."))
      .def("type_name", &compound_expr::type_name)
      .def("__repr__", &compound_expr::to_string)
      .def("__hash__", &hash<compound_expr>)
      .def("__eq__", &are_identical<compound_expr>, py::is_operator(),
           py::doc("Check if two compound expressions are strictly identical. This is not "
                   "mathematical equivalence."))
      .def("__bool__", [](const compound_expr&) {
        throw type_error("CompoundExpr cannot be coerced to boolean.");
      });

  m.def("create_compound_expression_elements", &create_expression_elements, py::arg("provenance"),
        py::arg("num"),
        py::doc("Create scalar expressions that represent the members of the provided compound "
                "expression."));

  m.def("create_custom_type_construction", &create_custom_type_construction, py::arg("type"),
        py::arg("expressions"),
        py::doc("Create compound expression of type `CustomTypeConstruction`."));
}

}  // namespace wf
