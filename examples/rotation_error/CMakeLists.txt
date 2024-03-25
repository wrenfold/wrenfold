add_py_code_generator(rotation_error_generate rotation_error.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(rotation_error_test GENERATOR_TARGET rotation_error_generate
             SOURCE_FILES rotation_error_test.cc)
