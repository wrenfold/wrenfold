// Copyright 2024 Gareth Cross

#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/boolean_expression.h"
#include "wf/constants.h"
#include "wf/expressions/special_constants.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

static bool coerce_to_bool(const boolean_expr& self) {
  if (const boolean_constant* constant = cast_ptr<const boolean_constant>(self);
      constant != nullptr) {
    return constant->value();
  } else {
    throw type_error(
        "Expression of type `{}` cannot be coerced to boolean. Only expressions sym.true "
        "and sym.false can be evaluated for truthiness.",
        self.type_name());
  }
}

void wrap_boolean_expression(py::module_& m) {
  wrap_class<boolean_expr>(m, "BooleanExpr")
      .def("__repr__", &boolean_expr::to_string)
      .def("expression_tree_str", &boolean_expr::to_expression_tree_string,
           py::doc("Retrieve the expression tree as a pretty-printed string."))
      .def_property_readonly("type_name", [](const boolean_expr& self) { return self.type_name(); })
      .def("__bool__", &coerce_to_bool, py::doc("Coerce expression to boolean."));

  // We need to declare these here so that boolean_expr is available in `m`.
  m.attr("true") = constants::boolean_true;
  m.attr("false") = constants::boolean_false;
}

}  // namespace wf