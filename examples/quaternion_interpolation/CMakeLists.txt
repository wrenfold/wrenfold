add_py_code_generator(quat_interpolation_generate quaternion_interpolation.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(quat_interpolation_test SOURCE_FILES quat_interpolation_test.cc
             GENERATOR_TARGET quat_interpolation_generate)
