// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <pybind11/pybind11.h>

#include "wf/distribute.h"  //  distribute_visitor

#include "docs/symbolic_functions_wrapper.h"
#include "visitor_wrappers.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

template <typename Visitor>
auto wrap_visitor_with_any_expression_arg() {
  return [](const any_const_ptr_to_expression& expr) {
    Visitor visitor{};
    return std::visit([&visitor](const auto& x) -> any_expression { return visitor(*x); }, expr);
  };
}

void wrap_symbolic_functions(py::module_& m) {
  m.def("distribute", wrap_visitor_with_any_expression_arg<distribute_visitor>(), py::arg("expr"),
        wf::docstrings::distribute.data());

  m.def("subs", &substitute_wrapper<any_const_ptr_to_expression>, py::arg("expr"), py::arg("pairs"),
        wf::docstrings::subs.data());
  m.def("subs", &substitute_wrapper_single<any_const_ptr_to_expression, scalar_expr>,
        py::arg("expr"), py::arg("target"), py::arg("replacement"),
        "Overload of ``subs`` that performs a single scalar-valued substitution.");
  m.def("subs", &substitute_wrapper_single<any_const_ptr_to_expression, boolean_expr>,
        py::arg("expr"), py::arg("target"), py::arg("replacement"),
        "Overload of ``subs`` that performs a single boolean-valued substitution.");
}

}  // namespace wf
