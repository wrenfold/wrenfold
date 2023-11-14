# Add test-support library
add_subdirectory(test_support)

# Add a single test.
function(add_custom_test SOURCE_FILE)
  # get just the filename w/o extension
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)

  # add executable for the test:
  message(STATUS "Adding test: ${TEST_NAME}")
  add_executable(${TEST_NAME} ${SOURCE_FILE} custom_main.cc)

  target_link_libraries(${TEST_NAME} ${PROJECT_PREFIX}-test-support)
  target_link_libraries(${TEST_NAME} ${PROJECT_PREFIX}-runtime gtest eigen
                        fmt::fmt-header-only)
  target_compile_options(${TEST_NAME} PRIVATE ${SHARED_WARNING_FLAGS})

  add_test(${TEST_NAME} ${TEST_NAME})
endfunction()

add_custom_test(plain_formatter_test.cc)
add_custom_test(scalar_operations_test.cc)
add_custom_test(derivatives_test.cc)
add_custom_test(integer_utils_test.cc)
add_custom_test(numeric_expressions_test.cc)
add_custom_test(functions_test.cc)
add_custom_test(matrix_operations_test.cc)
add_custom_test(ir_builder_test.cc)
add_custom_test(substitute_test.cc)
add_custom_test(quaternion_test.cc)
add_custom_test(limits_test.cc)
add_code_generation_test(generate_cpp_test cpp_generation_gen.cc
                         cpp_generation_test.cc)

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
  set(CRATE_ROOT "${CMAKE_SOURCE_DIR}/components/rust/rust-codegen-tests")

  # Find cargo
  find_program(CARGO_PATH cargo)
  if(DEFINED CARGO_PATH)
    message(STATUS "cargo path: ${CARGO_PATH}")
    # Add a target to do the cargo build:
    add_custom_target(
      ${NAME}_evaluate
      COMMAND
        ${CMAKE_COMMAND} -E env
        "CODE_GENERATION_FILE=${TEST_OUTPUT_DIR}/generated.rs"
        "CARGO_TARGET_DIR=${TEST_OUTPUT_DIR}/target" ${CARGO_PATH} test --
        --test-threads 1
      WORKING_DIRECTORY ${CRATE_ROOT}
      COMMENT "Cargo build for test ${NAME}"
      DEPENDS "${TEST_OUTPUT_DIR}/generated.rs" "${CRATE_ROOT}/src/lib.rs"
              "${CRATE_ROOT}/Cargo.toml")
  endif()
endfunction()

add_rust_generation_test(generate_rust_test rust_generation_gen.cc)
