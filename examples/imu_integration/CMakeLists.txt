add_py_code_generator(imu_integration_generate imu_integration.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(imu_integration_test GENERATOR_TARGET imu_integration_generate
             SOURCE_FILES imu_integration_test.cc)
