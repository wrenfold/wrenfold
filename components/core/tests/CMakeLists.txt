# Add a single-file test with the name derived from the CC File.
function(add_single_file_test SOURCE_FILE)
  get_filename_component(TEST_NAME ${SOURCE_FILE} NAME_WE)
  add_cpp_test(${TEST_NAME} SOURCE_FILES ${SOURCE_FILE})
endfunction()

add_single_file_test(algorithm_utils_test.cc)
add_single_file_test(bitset_range_test.cc)
add_single_file_test(checked_int_test.cc)
add_single_file_test(checked_pointers_test.cc)
add_single_file_test(compound_expressions_test.cc)
add_single_file_test(cse_visitor_test.cc)
add_single_file_test(derivatives_test.cc)
add_single_file_test(factorizer_test.cc)
add_single_file_test(functions_test.cc)
add_single_file_test(integer_utils_test.cc)
add_single_file_test(ir_conversion_test.cc)
add_single_file_test(matrix_operations_test.cc)
add_single_file_test(numeric_expressions_test.cc)
add_single_file_test(plain_formatter_test.cc)
add_single_file_test(quaternion_test.cc)
add_single_file_test(scalar_operations_test.cc)
add_single_file_test(stack_allocator_test.cc)
add_single_file_test(static_vector_test.cc)
add_single_file_test(string_utils_test.cc)
add_single_file_test(substitute_test.cc)
add_single_file_test(tree_formatter_test.cc)
add_single_file_test(type_list_test.cc)

add_compiled_code_generator(
  cpp_generation_generate
  OUTPUT_FILE_NAME
  "generated.h"
  SOURCE_FILES
  cpp_generation_gen.cc
  test_expressions.h
  test_expressions_codegen.h)

add_cpp_test(cpp_generation_test SOURCE_FILES cpp_generation_test.cc
             test_expressions.h GENERATOR_TARGET cpp_generation_generate)

add_compiled_code_generator(
  cpp_generation_generate_eigen
  OUTPUT_FILE_NAME
  "generated.h"
  SOURCE_FILES
  cpp_generation_gen.cc
  test_expressions.h
  test_expressions_codegen.h
  GENERATOR_DEFINITIONS
  -DUSE_EIGEN)

add_cpp_test(
  cpp_generation_eigen_test
  SOURCE_FILES
  cpp_generation_test.cc
  test_expressions.h
  GENERATOR_TARGET
  cpp_generation_generate_eigen
  TEST_DEFINITIONS
  -DUSE_EIGEN)

add_py_code_generator(
  cpp_generation_generate_2
  generation_test_2.py
  OUTPUT_FILE_NAME
  generated.h
  SCRIPT_ARGUMENTS
  --language
  cpp)

add_cpp_test(cpp_generation_test_2 SOURCE_FILES cpp_generation_test_2.cc
             GENERATOR_TARGET cpp_generation_generate_2)

# Rust tests
add_compiled_code_generator(
  rust_generation_generate
  OUTPUT_FILE_NAME
  "generated.rs"
  SOURCE_FILES
  rust_generation_gen.cc
  test_expressions.h
  test_expressions_codegen.h)
copy_generated_file(GENERATOR_TARGET rust_generation_generate DESTINATION_DIR
                    rust_generation_test/src TARGET_GROUP wf_rust_generation)

# Check `generated.rs` for diffs.
add_python_test(generation_diff_test.py SCRIPT_ARGUMENTS
                rust_generation_test/src/generated.rs)

add_py_code_generator(
  rust_generation_generate_2
  generation_test_2.py
  OUTPUT_FILE_NAME
  generated.rs
  SCRIPT_ARGUMENTS
  --language
  rust)
copy_generated_file(GENERATOR_TARGET rust_generation_generate_2 DESTINATION_DIR
                    rust_generation_test_2/src TARGET_GROUP wf_rust_generation)
