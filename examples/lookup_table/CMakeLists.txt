add_py_code_generator(lookup_table_generate_cpp lookup_table_gen.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(lookup_table_cpp_test GENERATOR_TARGET lookup_table_generate_cpp
             SOURCE_FILES lookup_table_test.cc)
