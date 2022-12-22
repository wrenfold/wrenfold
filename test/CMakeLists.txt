set(TEST_LIBRARY_NAME ${PROJECT_NAME}_TEST_UTIL)
set(${TEST_LIBRARY_NAME}_SOURCES "test_helpers.cc")
add_library(${TEST_LIBRARY_NAME} STATIC ${${TEST_LIBRARY_NAME}_SOURCES})
target_link_libraries(${TEST_LIBRARY_NAME} ${LIBRARY_NAME} gtest_main fmt)
target_compile_options(${TEST_LIBRARY_NAME} PUBLIC ${MATH_COMPILATION_FLAGS})

# Find all the test files.
file(GLOB ${PROJECT_NAME}_TEST_SOURCES "*_test.cc")
foreach(test_file ${${PROJECT_NAME}_TEST_SOURCES})
  # get just the filename w/o extension
  get_filename_component(test_name ${test_file} NAME_WE)

  # add test executable for every file
  message(STATUS "Discovered test: ${test_name}")
  add_executable(${test_name} ${test_file})
  target_link_libraries(${test_name} ${TEST_LIBRARY_NAME} gtest_main fmt)

  add_test(${test_name} ${test_name})
endforeach(test_file)
