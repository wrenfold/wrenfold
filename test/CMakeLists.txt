set(TEST_LIBRARY_NAME ${PROJECT_NAME}_TEST_UTIL)
add_library(${TEST_LIBRARY_NAME} STATIC test_helpers.cc)
target_link_libraries(${TEST_LIBRARY_NAME} ${LIBRARY_NAME} gtest_main fmt)
target_compile_options(${TEST_LIBRARY_NAME} PUBLIC ${MATH_COMPILATION_FLAGS})

# Specify individual tests here:
set(${PROJECT_NAME}_TEST_SOURCES
    plain_formatter_test.cc scalar_operations_test.cc
    scalar_derivatives_test.cc integer_utils_test.cc
    numeric_expressions_test.cc functions_test.cc)

foreach(test_file ${${PROJECT_NAME}_TEST_SOURCES})
  # get just the filename w/o extension
  get_filename_component(test_name ${test_file} NAME_WE)

  # add test executable for every file
  message(STATUS "Discovered test: ${test_name}")
  add_executable(${test_name} ${test_file})
  target_link_libraries(${test_name} ${TEST_LIBRARY_NAME} gtest_main fmt)

  add_test(${test_name} ${test_name})
endforeach(test_file)
