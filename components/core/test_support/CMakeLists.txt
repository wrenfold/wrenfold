# Create a library with reusable test utilities.
set(TEST_LIBRARY_NAME ${PROJECT_PREFIX}-test-support)
add_library(
  ${TEST_LIBRARY_NAME}
  INTERFACE
  wf_test_support/code_gen_helpers.h
  wf_test_support/eigen_test_macros.h
  wf_test_support/numeric_testing.h
  wf_test_support/test_macros.h
  wf_test_support/numerical_jacobian.h)

target_include_directories(
  ${TEST_LIBRARY_NAME} INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                                 $<INSTALL_INTERFACE:include>)

if(NOT TARGET ${LIBRARY_NAME})
  message(FATAL_ERROR "Main library target has not been defined.")
endif()

target_link_libraries(
  ${TEST_LIBRARY_NAME} INTERFACE ${LIBRARY_NAME} ${PROJECT_PREFIX}-runtime
                                 gtest eigen fmt::fmt-header-only)

# Object target for custom main() for gtest
set(CUSTOM_MAIN ${PROJECT_PREFIX}-custom-main)
add_library(${CUSTOM_MAIN} OBJECT custom_main.cc)
target_link_libraries(${CUSTOM_MAIN} gtest)
