// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>  // Required to pass std::vector.

#include "wf/code_generation/types.h"  //  Required for definition of custom_type.
#include "wf/compound_expression.h"

#include "docs/compound_expression_wrapper.h"
#include "wf/expressions/custom_type_expressions.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

void wrap_compound_expression(py::module_& m) {
  wrap_class<compound_expr>(m, "CompoundExpr")
      .def_property_readonly(
          "type_name", [](const compound_expr& self) { return self.type_name(); },
          "Retrieve the name of the underlying C++ expression type. See "
          ":func:`wrenfold.sym.Expr.type_name`.")
      .def("__repr__", &compound_expr::to_string)
      .def("__bool__",
           [](const compound_expr&) {
             throw type_error("CompoundExpr cannot be coerced to boolean.");
           })
      .doc() = docstrings::compound_expr.data();

  m.def("create_compound_expression_elements", &create_expression_elements, py::arg("provenance"),
        py::arg("num"),
        py::doc("Create scalar expressions that represent the members of the provided compound "
                "expression. OMIT_FROM_SPHINX"));

  m.def(
      "create_custom_type_construction",
      [](const custom_type& type, const py::sequence& expressions) {
        return custom_type_construction::create(
            type, transform_map<custom_type_construction::container_type>(
                      expressions, &variant_from_pyobject<any_expression>));
      },
      py::arg("type"), py::arg("expressions"),
      py::doc("Create compound expression of type `CustomTypeConstruction`. OMIT_FROM_SPHINX"));
}

}  // namespace wf
