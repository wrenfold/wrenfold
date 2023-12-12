# Add a single test.
function(add_custom_test SOURCE_FILE)
  # get just the filename w/o extension
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)

  # add executable for the test:
  message(STATUS "Adding test: ${TEST_NAME}")
  add_executable(${TEST_NAME} ${SOURCE_FILE} $<TARGET_OBJECTS:wf-custom-main>)

  target_link_libraries(${TEST_NAME} wf-test-support)
  target_link_libraries(${TEST_NAME} wf-runtime gtest eigen
                        fmt::fmt-header-only)
  target_compile_options(${TEST_NAME} PRIVATE ${SHARED_WARNING_FLAGS})

  if(NOT MSVC)
    target_compile_options(${TEST_NAME} PRIVATE -Wno-unused-comparison)
  endif()

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

add_code_generation_test(
  cpp_generation
  GENERATOR_SOURCE_FILES
  cpp_generation_gen.cc
  test_expressions.h
  EVALUATOR_SOURCE_FILES
  cpp_generation_test.cc
  test_expressions.h)

if(${ENABLE_RUST_TARGETS})
  add_rust_generation_test(
    NAME
    rust_generation
    GENERATOR_SOURCE_FILES
    rust_generation_gen.cc
    test_expressions.h
    CRATE_NAME
    rust_generation_test
    CRATE_SOURCES
    rust_generation_test/src/lib.rs
    rust_generation_test/src/gen.rs)
endif()
