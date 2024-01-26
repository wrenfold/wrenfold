add_py_code_generator(py_rotation_error_generate rotation_error.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(rotation_error_test GENERATOR_TARGET py_rotation_error_generate
             SOURCE_FILES py_rotation_error_test.cc)
