add_py_code_generator(
  py_bspline_generate_cpp
  bspline.py
  OUTPUT_FILE_NAME
  generated.h
  SCRIPT_ARGUMENTS
  --language
  cpp)

add_cpp_test(bspline_cpp_test GENERATOR_TARGET py_bspline_generate_cpp
             SOURCE_FILES py_bspline_test.cc)

add_py_code_generator(
  py_bspline_generate_rust
  bspline.py
  OUTPUT_FILE_NAME
  generated.rs
  SCRIPT_ARGUMENTS
  --language
  rust)

add_rust_test(
  bspline_rust_test
  CRATE_NAME
  bspline_rust_test
  GENERATOR_TARGET
  py_bspline_generate_rust
  CRATE_SOURCES
  bspline_rust_test/src/lib.rs)
