if(NOT DEFINED Python_EXECUTABLE)
  message(FATAL_ERROR "The Python executable could not be located.")
endif()

# Function that defines a new python test.
function(add_python_test PYTHON_SOURCE_FILE)
  get_filename_component(TEST_NAME ${PYTHON_SOURCE_FILE} NAME_WE)
  if(WIN32)
    set(PATH_SEP ";")
  else()
    set(PATH_SEP ":")
  endif()
  # In order for `PYTHONPATH` to be set correctly, we need to pass the
  # environment variable using the cmake command. No other mechanism I have
  # tried will work here.
  add_test(
    NAME ${TEST_NAME}
    COMMAND
      ${CMAKE_COMMAND} -E env
      "PYTHONPATH=${COMPONENTS_BINARY_DIR}${PATH_SEP}${COMPONENTS_SOURCE_DIR}/python"
      ${Python_EXECUTABLE} -B ${CMAKE_CURRENT_SOURCE_DIR}/${PYTHON_SOURCE_FILE})
  message(STATUS "Added python test: ${TEST_NAME}")
endfunction()

add_python_test(expression_wrapper_test.py)
add_python_test(matrix_wrapper_test.py)
add_python_test(codegen_wrapper_test.py)
