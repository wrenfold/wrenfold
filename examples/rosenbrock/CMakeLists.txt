add_py_code_generator(rosenbrock_generate rosenbrock.py OUTPUT_FILE_NAME
                      generated.h)

add_cpp_test(rosenbrock_test SOURCE_FILES rosenbrock_test.cc GENERATOR_TARGET
             rosenbrock_generate)
