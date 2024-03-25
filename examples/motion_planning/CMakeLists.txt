add_py_code_generator(motion_planning_generate problem.py OUTPUT_FILE_NAME
                      generated.rs)

copy_generated_file(GENERATOR_TARGET motion_planning_generate DESTINATION_DIR
                    motion_planning_test/src TARGET_GROUP wf_rust_generation)
