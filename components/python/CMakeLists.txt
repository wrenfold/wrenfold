# We make a target to store the list of python sources.
add_library(
  wf_python
  INTERFACE
  wrenfold/__init__.py
  wrenfold/code_generation.py
  wrenfold/geometry.py
  wrenfold/sym.py
  wrenfold/type_annotations.py)
