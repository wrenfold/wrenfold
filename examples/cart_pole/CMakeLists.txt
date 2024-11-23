add_python_test(cart_pole_test.py SOURCE_FILES cart_pole_dynamics.py
                sympy_helpers.py)

add_py_code_generator(cart_pole_generator_rust cart_pole_dynamics.py
                      OUTPUT_FILE_NAME generated.rs)

copy_generated_file(GENERATOR_TARGET cart_pole_generator_rust DESTINATION_DIR
                    cart_pole_demo/src TARGET_GROUP wf_rust_generation)
