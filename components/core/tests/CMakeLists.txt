# Add a single-file test with the name derived from the CC File.
function(add_single_file_test SOURCE_FILE)
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)
  add_cpp_test(${TEST_NAME} SOURCE_FILES ${SOURCE_FILE})
endfunction()

add_single_file_test(plain_formatter_test.cc)
add_single_file_test(scalar_operations_test.cc)
add_single_file_test(derivatives_test.cc)
add_single_file_test(integer_utils_test.cc)
add_single_file_test(numeric_expressions_test.cc)
add_single_file_test(functions_test.cc)
add_single_file_test(matrix_operations_test.cc)
add_single_file_test(ir_builder_test.cc)
add_single_file_test(substitute_test.cc)
add_single_file_test(quaternion_test.cc)
add_single_file_test(limits_test.cc)

add_code_generation_test(
  cpp_generation
  GENERATOR_SOURCE_FILES
  cpp_generation_gen.cc
  test_expressions.h
  EVALUATOR_SOURCE_FILES
  cpp_generation_test.cc
  test_expressions.h)

if(${ENABLE_RUST_TARGETS})

  add_compiled_code_generator(
    rust_generation_generate OUTPUT_FILE_NAME "generated.rs" SOURCE_FILES
    rust_generation_gen.cc test_expressions.h)

  add_rust_test(
    rust_generation_test
    GENERATOR_TARGET
    rust_generation_generate
    CRATE_NAME
    rust_generation_test
    CRATE_SOURCES
    rust_generation_test/src/lib.rs
    rust_generation_test/src/gen.rs)
endif()
