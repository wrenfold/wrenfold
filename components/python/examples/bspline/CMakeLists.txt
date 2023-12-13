add_py_code_generator(py_bspline_generate bspline.py OUTPUT_FILE_NAME
                      generated.h)

add_cpp_test(py_bspline_test GENERATOR_TARGET py_bspline_generate SOURCE_FILES
             py_bspline_test.cc)
