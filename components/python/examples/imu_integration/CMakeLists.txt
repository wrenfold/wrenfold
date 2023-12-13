add_py_code_generator(py_imu_integration_generate imu_integration.py
                      OUTPUT_FILE_NAME generated.h)

add_cpp_test(
  py_imu_integration_test GENERATOR_TARGET py_imu_integration_generate
  SOURCE_FILES py_imu_integration_test.cc)
