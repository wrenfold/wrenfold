# Define a code-generation test. Each test consists of two stages: First the
# `xxx_generate` target generates a header named `generated.h`, and then the
# `xxx_evaluate` target includes the generated code and runs tests.
function(add_code_generation_test NAME GENERATION_SOURCE_FILE TEST_SOURCE_FILE)
  # Parse args to get the `CODE_GEN_TEST_SHARED_SOURCE_FILES` arg:
  set(options "")
  set(oneValueArgs "")
  set(multiValueArgs SHARED_SOURCE_FILES)
  cmake_parse_arguments(CODE_GEN_TEST "${options}" "${oneValueArgs}"
                        "${multiValueArgs}" ${ARGN})

  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

  add_executable(${generate_target} ${GENERATION_SOURCE_FILE}
                                    ${CODE_GEN_TEST_SHARED_SOURCE_FILES})
  target_link_libraries(${generate_target} ${PROJECT_PREFIX}-test-support)
  target_include_directories(${generate_target} PRIVATE ${TEST_OUTPUT_DIR})

  # Add a target that runs the generation:
  add_custom_command(
    OUTPUT "${TEST_OUTPUT_DIR}/generated.h"
    COMMAND
      "$<TARGET_FILE_DIR:${generate_target}>/$<TARGET_FILE_NAME:${generate_target}>"
    WORKING_DIRECTORY "${TEST_OUTPUT_DIR}"
    COMMENT "Run code-generation for test ${NAME}"
    DEPENDS ${generate_target})

  add_custom_target(${generate_target}_run
                    DEPENDS "${TEST_OUTPUT_DIR}/generated.h")

  # Then create a target that evaluates the result: TODO: Move custom_main.cc
  # somewhere more common?
  set(evaluate_target ${NAME}_test)
  add_executable(
    ${evaluate_target}
    ${TEST_SOURCE_FILE} ${CODE_GEN_TEST_SHARED_SOURCE_FILES}
    "${CMAKE_SOURCE_DIR}/components/math/tests/custom_main.cc"
    "${TEST_OUTPUT_DIR}/generated.h")
  target_link_libraries(${evaluate_target} ${PROJECT_PREFIX}-runtime
                        ${PROJECT_PREFIX}-test-support eigen gtest)
  target_include_directories(${evaluate_target} PRIVATE ${TEST_OUTPUT_DIR})

  # The evaluation target must depend on running the code-generation
  add_dependencies(${evaluate_target} ${generate_target}_run)

  # Create test case from the evaluation target:
  add_test(${evaluate_target} ${evaluate_target})
endfunction()

# Define a code-generation test that performs the code generation step in
# python, then runs a C++ unit test.
function(add_py_code_generation_test NAME GENERATION_SOURCE_FILE
         TEST_SOURCE_FILE)
  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")
  set(TEST_OUTPUT_PATH "${TEST_OUTPUT_DIR}/generated.h")

  if(WIN32)
    set(PATH_SEP ";")
  else()
    set(PATH_SEP ":")
  endif()

  if(NOT DEFINED Python_EXECUTABLE)
    message(FATAL_ERROR "The Python executable could not be located.")
  endif()

  # Add a target that runs the generation:
  set(PYTHON_SOURCE_FILE
      "${CMAKE_CURRENT_SOURCE_DIR}/${GENERATION_SOURCE_FILE}")
  add_custom_command(
    OUTPUT ${TEST_OUTPUT_PATH}
    COMMAND
      ${CMAKE_COMMAND} -E env
      "PYTHONPATH=${COMPONENTS_BINARY_DIR}${PATH_SEP}${COMPONENTS_SOURCE_DIR}/python"
      ${Python_EXECUTABLE} -B ${PYTHON_SOURCE_FILE}
    WORKING_DIRECTORY "${TEST_OUTPUT_DIR}"
    COMMENT "Run python code-generation for test ${NAME}"
    DEPENDS ${LIBRARY_NAME} ${PROJECT_PREFIX}_pysym ${PYTHON_SOURCE_FILE})

  add_custom_target(${generate_target} DEPENDS ${TEST_OUTPUT_PATH})

  # Then create a target that evaluates the result:
  set(evaluate_target ${NAME}_test)
  add_executable(
    ${evaluate_target}
    ${TEST_SOURCE_FILE}
    "${CMAKE_SOURCE_DIR}/components/math/tests/custom_main.cc"
    ${TEST_OUTPUT_PATH})
  target_link_libraries(${evaluate_target} ${PROJECT_PREFIX}-runtime
                        ${PROJECT_PREFIX}-test-support eigen gtest)
  target_include_directories(${evaluate_target} PRIVATE ${TEST_OUTPUT_DIR})

  # The evaluation target must depend on running the code-generation
  add_dependencies(${evaluate_target} ${generate_target})

  # Create test case from the evaluation target:
  add_test(${evaluate_target} ${evaluate_target})
endfunction()

# Define a new python test.
function(add_python_test PYTHON_SOURCE_FILE)
  get_filename_component(TEST_NAME ${PYTHON_SOURCE_FILE} NAME_WE)
  if(WIN32)
    set(PATH_SEP ";")
  else()
    set(PATH_SEP ":")
  endif()

  if(NOT DEFINED Python_EXECUTABLE)
    message(FATAL_ERROR "The Python executable could not be located.")
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
