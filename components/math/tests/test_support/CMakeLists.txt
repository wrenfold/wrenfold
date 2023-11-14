# Create a library with reusable test utilities.
set(TEST_LIBRARY_NAME ${PROJECT_PREFIX}-test-support)
add_library(
  ${TEST_LIBRARY_NAME} STATIC
  test_helpers.cc code_gen_helpers.h eigen_test_helpers.h numeric_testing.h
  test_helpers.h numerical_jacobian.h)

target_include_directories(
  ${TEST_LIBRARY_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                              $<INSTALL_INTERFACE:include>)

if(NOT TARGET ${LIBRARY_NAME})
  message(FATAL_ERROR "Main library target has not been defined.")
endif()

target_link_libraries(
  ${TEST_LIBRARY_NAME} ${LIBRARY_NAME} ${PROJECT_PREFIX}-runtime gtest eigen
  fmt::fmt-header-only)
target_compile_options(${TEST_LIBRARY_NAME} PRIVATE ${SHARED_WARNING_FLAGS})
