add_py_code_generator(
  py_bspline_generate_cpp
  bspline.py
  OUTPUT_FILE_NAME
  generated.h
  SCRIPT_ARGUMENTS
  --language
  cpp)

add_cpp_test(bspline_test GENERATOR_TARGET py_bspline_generate_cpp SOURCE_FILES
             bspline_test.cc bspline_numerical.h)

add_py_code_generator(
  py_bspline_generate_rust
  bspline.py
  OUTPUT_FILE_NAME
  generated.rs
  SCRIPT_ARGUMENTS
  --language
  rust)

copy_generated_file(GENERATOR_TARGET py_bspline_generate_rust DESTINATION_DIR
                    bspline_rust_test/src TARGET_GROUP wf_rust_generation)
