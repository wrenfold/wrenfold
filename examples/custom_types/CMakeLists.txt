add_py_code_generator(
  custom_types_generate_cpp
  custom_types_gen.py
  OUTPUT_FILE_NAME
  generated.h
  SCRIPT_ARGUMENTS
  --language
  cpp)

add_cpp_test(custom_types_cpp_test GENERATOR_TARGET custom_types_generate_cpp
             SOURCE_FILES custom_types_test.cc)

add_py_code_generator(
  custom_types_generate_rust
  custom_types_gen.py
  OUTPUT_FILE_NAME
  generated.rs
  SCRIPT_ARGUMENTS
  --language
  rust)

copy_generated_file(GENERATOR_TARGET custom_types_generate_rust DESTINATION_DIR
                    custom_types_rust_test/src TARGET_GROUP wf_rust_generation)
