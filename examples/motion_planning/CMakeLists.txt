add_py_code_generator(py_motion_planning problem.py OUTPUT_FILE_NAME
                      generated.rs)

copy_generated_file(GENERATOR_TARGET py_motion_planning DESTINATION_DIR
                    motion_planning_test/src TARGET_GROUP wf_rust_generation)
