# We make a target to store the list of python sources.
add_library(
  wf_python
  INTERFACE
  wrenfold/__init__.py
  wrenfold/ast.py
  wrenfold/code_generation.py
  wrenfold/custom_types.py
  wrenfold/enumerations.py
  wrenfold/exceptions.py
  wrenfold/expressions.py
  wrenfold/external_functions.py
  wrenfold/geometry.py
  wrenfold/sym.py
  wrenfold/sympy_conversion.py
  wrenfold/type_annotations.py
  wrenfold/type_info.py)
