# Define a code-generation test. Each test consists of two stages: First the
# `xxx_generate` target generates a header named `generated.h`, and then the
# `xxx_evaluate` target includes the generated code and runs tests.
function(add_code_generation_test NAME)
  # Parse args to get the `TEST_GENERATE_SOURCE_FILES` and
  # `TEST_EVAL_SOURCE_FILES`.
  set(options "")
  set(oneValueArgs "")
  set(multiValueArgs GENERATE_SOURCE_FILES EVAL_SOURCE_FILES)
  cmake_parse_arguments(TEST "${options}" "${oneValueArgs}" "${multiValueArgs}"
                        ${ARGN})

  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

  add_executable(${generate_target} ${TEST_GENERATE_SOURCE_FILES})
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

  # Then create a target that evaluates the result.
  set(evaluate_target ${NAME}_test)
  add_executable(
    ${evaluate_target}
    ${TEST_EVAL_SOURCE_FILES} $<TARGET_OBJECTS:${PROJECT_PREFIX}-custom-main>
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
      "PYTHONPATH=${COMPONENTS_BINARY_DIR}/wrapper${PATH_SEP}${COMPONENTS_SOURCE_DIR}/python"
      ${Python_EXECUTABLE} -B ${PYTHON_SOURCE_FILE}
    WORKING_DIRECTORY "${TEST_OUTPUT_DIR}"
    COMMENT "Run python code-generation for test ${NAME}"
    DEPENDS ${LIBRARY_NAME} ${PROJECT_PREFIX}_wrapper ${PYTHON_SOURCE_FILE})

  add_custom_target(${generate_target} DEPENDS ${TEST_OUTPUT_PATH})

  # Then create a target that evaluates the result:
  set(evaluate_target ${NAME}_test)
  add_executable(
    ${evaluate_target}
    ${TEST_SOURCE_FILE} $<TARGET_OBJECTS:${PROJECT_PREFIX}-custom-main>
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
      "PYTHONPATH=${COMPONENTS_BINARY_DIR}/wrapper${PATH_SEP}${COMPONENTS_SOURCE_DIR}/python"
      ${Python_EXECUTABLE} -B ${CMAKE_CURRENT_SOURCE_DIR}/${PYTHON_SOURCE_FILE})
  message(STATUS "Added python test: ${TEST_NAME}")
endfunction()

# Function to define a test that generates in C++ and then runs Rust tests.
function(add_rust_generation_test NAME GENERATION_SOURCE_FILE)
  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

  add_executable(${generate_target} ${GENERATION_SOURCE_FILE})
  target_link_libraries(${generate_target} ${LIBRARY_NAME}
                        ${PROJECT_PREFIX}-test-support)
  target_compile_options(${generate_target} PRIVATE ${SHARED_WARNING_FLAGS})

  # Add a target that runs the generation:
  add_custom_command(
    OUTPUT "${TEST_OUTPUT_DIR}/generated.rs"
    COMMAND
      "$<TARGET_FILE_DIR:${generate_target}>/$<TARGET_FILE_NAME:${generate_target}>"
    WORKING_DIRECTORY "${TEST_OUTPUT_DIR}"
    COMMENT "Run code-generation for test ${NAME}"
    DEPENDS ${generate_target})

  # Path to the crate that implements the rust tests
  set(RUST_DIR "${CMAKE_SOURCE_DIR}/components/rust")
  set(TEST_CRATE_ROOT "${RUST_DIR}/rust-codegen-tests")

  set(RUST_SOURCES "${TEST_CRATE_ROOT}/Cargo.toml"
                   "${TEST_CRATE_ROOT}/src/lib.rs")

  # Find cargo
  find_program(CARGO_PATH cargo)
  if(DEFINED CARGO_PATH)
    message(STATUS "cargo path: ${CARGO_PATH}")
    # Add a target to do the cargo build:
    add_custom_target(
      ${NAME}_test
      COMMAND
        ${CMAKE_COMMAND} -E env
        "CODE_GENERATION_FILE=${TEST_OUTPUT_DIR}/generated.rs"
        "CARGO_TARGET_DIR=${TEST_OUTPUT_DIR}/target" ${CARGO_PATH} test --
        --test-threads 1
      WORKING_DIRECTORY ${TEST_CRATE_ROOT}
      COMMENT "Cargo build for test ${NAME}"
      DEPENDS "${TEST_OUTPUT_DIR}/generated.rs" ${RUST_SOURCES})
  endif()
endfunction()
