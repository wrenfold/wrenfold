// Copyright 2024 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Defined in scalar_wrapper.cc
void wrap_scalar_operations(py::module_& m);

// Defined in matrix_wrapper.cc
void wrap_matrix_operations(py::module_& m);

// Defined in codegen_wrapper.cc
void wrap_codegen_operations(py::module_& m);
void wrap_argument(py::module_& m);

// Defined in ast_wrapper.cc
void wrap_ast(py::module_& m);

// Defined in types_wrapper.cc
void wrap_types(py::module_& m);

// Defined in code_formatting_wrapper.cc
void wrap_code_formatting_operations(py::module_& m);

// Defined in compound_expression_wrapper.cc
void wrap_compound_expression(py::module_& m);

// Defined in boolean_expression_wrapper.cc
void wrap_boolean_expression(py::module_& m);

// Defined in geometry_wrapper.cc
void wrap_geometry_operations(py::module_& m);

// Defined in sympy_conversion.cc
void wrap_sympy_conversion(py::module_& m);

// Defined in enums_wrapper.cc
void wrap_enums(py::module_& m);

// Defined in exceptions_wrapper.cc
void wrap_exceptions(py::module_& m);
}  // namespace wf

PYBIND11_MODULE(PY_MODULE_NAME, m) {
  using namespace wf;
  auto m_enumerations = m.def_submodule(PY_SUBMODULE_NAME_ENUMERATIONS, "Wrapped enums.");
  wrap_enums(m_enumerations);

  auto m_exceptions = m.def_submodule(PY_SUBMODULE_NAME_EXCEPTIONS, "Wrapped exception types.");
  wrap_exceptions(m_exceptions);

  // For types to show up correctly in docstrings, we need to wrap `boolean_expr` first.
  auto m_sym = m.def_submodule(PY_SUBMODULE_NAME_SYM, "Wrapped mathematical operations.");
  wrap_boolean_expression(m_sym);
  wrap_scalar_operations(m_sym);
  wrap_matrix_operations(m_sym);
  wrap_compound_expression(m_sym);

  auto m_sympy_conversion =
      m.def_submodule(PY_SUBMODULE_NAME_SYMPY_CONVERSION, "Wrapped sympy conversion methods.");
  wrap_sympy_conversion(m_sympy_conversion);

  auto m_geo = m.def_submodule(PY_SUBMODULE_NAME_GEOMETRY, "Wrapped geometry methods.");
  wrap_geometry_operations(m_geo);

  auto m_types = m.def_submodule(PY_SUBMODULE_NAME_TYPE_INFO, "Wrapped code-generation types.");
  wrap_types(m_types);

  // We need to wrap `Argument` and `ArgumentDirection` first so they are available for `ast`.
  auto m_gen = m.def_submodule(PY_SUBMODULE_NAME_GEN, "Wrapped code-generation methods.");
  wrap_argument(m_gen);

  auto m_ast = m.def_submodule(PY_SUBMODULE_NAME_AST, "Wrapped AST types.");
  wrap_ast(m_ast);

  wrap_codegen_operations(m_gen);
  wrap_code_formatting_operations(m_gen);
}  // PYBIND11_MODULE