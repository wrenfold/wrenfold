add_code_generation_test(
  quat_interpolation
  GENERATOR_SOURCE_FILES
  quat_interpolation_gen.cc
  quat_interpolation_expressions.h
  EVALUATOR_SOURCE_FILES
  quat_interpolation_test.cc
  quat_interpolation_expressions.h)
