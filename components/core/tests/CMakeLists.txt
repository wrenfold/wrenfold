# Add a single test.
function(add_custom_test SOURCE_FILE)
  # get just the filename w/o extension
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)

  # add executable for the test:
  message(STATUS "Adding test: ${TEST_NAME}")
  add_executable(${TEST_NAME} ${SOURCE_FILE}
                              $<TARGET_OBJECTS:${PROJECT_PREFIX}-custom-main>)

  target_link_libraries(${TEST_NAME} ${PROJECT_PREFIX}-test-support)
  target_link_libraries(${TEST_NAME} ${PROJECT_PREFIX}-runtime gtest eigen
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
  GENERATE_SOURCE_FILES
  cpp_generation_gen.cc
  test_expressions.h
  EVAL_SOURCE_FILES
  cpp_generation_test.cc
  test_expressions.h)

add_rust_generation_test(rust_generation rust_generation_gen.cc)
