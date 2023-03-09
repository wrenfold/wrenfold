# Specify individual tests here:
set(${PROJECT_NAME}_PY_TEST_SOURCES
    expression_wrapper_test.py matrix_wrapper_test.py codegen_wrapper_test.py)

if(NOT DEFINED Python_EXECUTABLE)
  message(FATAL_ERROR "The Python executable could not be located.")
endif()

foreach(test_file ${${PROJECT_NAME}_PY_TEST_SOURCES})
  get_filename_component(test_name ${test_file} NAME_WE)
  # Run python test in the wrapper output directory. TODO: Is there a better way
  # of having the module on the path?
  add_test(NAME ${test_name} COMMAND ${Python_EXECUTABLE} -B
                                     ${CMAKE_CURRENT_SOURCE_DIR}/${test_file})
  set_property(
    TEST ${test_name}
    PROPERTY ENVIRONMENT
             "PYTHONPATH=${CMAKE_BINARY_DIR}/wrapper:$ENV{PYTHONPATH}")
endforeach(test_file)
