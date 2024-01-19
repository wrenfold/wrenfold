# Add a single-file test with the name derived from the CC File.
function(add_single_file_test SOURCE_FILE)
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)
  add_cpp_test(${TEST_NAME} SOURCE_FILES ${SOURCE_FILE})
endfunction()

add_single_file_test(algorithm_utils_test.cc)
add_single_file_test(checked_pointers_test.cc)
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
add_single_file_test(type_list_test.cc)

add_compiled_code_generator(
  cpp_generation_generate OUTPUT_FILE_NAME "generated.h" SOURCE_FILES
  cpp_generation_gen.cc test_expressions.h)

add_cpp_test(cpp_generation_test SOURCE_FILES cpp_generation_test.cc
             test_expressions.h GENERATOR_TARGET cpp_generation_generate)

add_compiled_code_generator(
  rust_generation_generate OUTPUT_FILE_NAME "generated.rs" SOURCE_FILES
  rust_generation_gen.cc test_expressions.h)

copy_generated_file(GENERATOR_TARGET rust_generation_generate DESTINATION_DIR
                    rust_generation_test/src TARGET_GROUP wf_rust_generation)
