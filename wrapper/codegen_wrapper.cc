// Copyright 2023 Gareth Cross

#define PYBIND11_DETAILED_ERROR_MESSAGES

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "code_generation.h"
#include "expression.h"

namespace py = pybind11;
using namespace py::literals;

namespace math {

void WrapCodeGenerationOperations(py::module_& m) {
  py::class_<ir::Value>(m, "IrValue").def("__repr__", [](const ir::Value& v) {
    return fmt::format("IrValue({})", v.Id());
  });

  // TODO: Not sure if this needs to be wrapped, but it might be convenient for now.
  py::class_<IrBuilder>(m, "IrBuilder")
      // Construct from a vector of expressions:
      .def(py::init<const std::vector<Expr>&>())
      .def("output_values", &IrBuilder::OutputValues, py::doc("Retrieve list of output values."))
      .def("format_ir", &IrBuilder::FormatIR,
           py::doc("Format the IR for a specific output value into a string."))
      .def("create_expression", &IrBuilder::CreateExpression,
           py::doc("Rebuild the expression tree for a given IR value."))
      .def("eliminate_duplicates", &IrBuilder::EliminateDuplicates,
           py::doc("Eliminate duplicate sub-expressions."));
}

}  // namespace math
