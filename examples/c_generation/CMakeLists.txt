add_py_code_generator(
  c_generation_generate
  c_generation.py
  OUTPUT_FILE_NAME
  generated.h
  SOURCE_FILES
  c_generation.py
  c_code_generator.py
  ${WF_EXAMPLES_DIR}/shared_expressions.py)

add_cpp_test(
  c_generation_test
  GENERATOR_TARGET
  c_generation_generate
  SOURCE_FILES
  c_generation_test.cc
  c_span_types.h
  c_generation.c
  INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR})

set_property(TARGET c_generation_test PROPERTY C_STANDARD 11)
