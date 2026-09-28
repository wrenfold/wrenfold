add_py_code_generator(unit3_error_generate unit3_error.py OUTPUT_FILE_NAME
                      generated.h)

add_cpp_test(unit3_error_test GENERATOR_TARGET unit3_error_generate
             SOURCE_FILES unit3_error_test.cc)
