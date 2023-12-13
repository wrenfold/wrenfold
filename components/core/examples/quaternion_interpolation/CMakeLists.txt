add_compiled_code_generator(
  quat_interpolation_generate OUTPUT_FILE_NAME "generated.h" SOURCE_FILES
  quat_interpolation_gen.cc quat_interpolation_expressions.h)

add_cpp_test(
  quat_interpolation_test SOURCE_FILES quat_interpolation_test.cc
  quat_interpolation_expressions.h GENERATOR_TARGET quat_interpolation_generate)
