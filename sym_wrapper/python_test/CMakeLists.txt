# Specify individual tests here:
set(${PROJECT_NAME}_PY_TEST_SOURCES
    expression_wrapper_test.py matrix_wrapper_test.py codegen_wrapper_test.py)

if(NOT DEFINED Python_EXECUTABLE)
  message(FATAL_ERROR "The Python executable could not be located.")
endif()

add_test(NAME run_python_tests
         COMMAND ${Python_EXECUTABLE} -B
                 ${CMAKE_CURRENT_SOURCE_DIR}/run_python_tests.py)
set_tests_properties(
  run_python_tests PROPERTIES ENVIRONMENT
                              "CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR}")
