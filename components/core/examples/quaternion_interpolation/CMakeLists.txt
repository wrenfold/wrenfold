add_code_generation_test(
  quat_interpolation
  GENERATE_SOURCE_FILES
  quat_interpolation_gen.cc
  quat_interpolation_expressions.h
  EVAL_SOURCE_FILES
  quat_interpolation_test.cc
  quat_interpolation_expressions.h)
