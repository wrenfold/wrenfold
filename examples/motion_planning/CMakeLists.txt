add_py_code_generator(
  py_motion_planning
  problem.py
  OUTPUT_FILE_NAME
  generated.rs)

add_rust_test(
  motion_planning_test
  CRATE_NAME
  motion_planning_test
  GENERATOR_TARGET
  py_motion_planning
  CRATE_SOURCES
  motion_planning_test/src/lib.rs)
