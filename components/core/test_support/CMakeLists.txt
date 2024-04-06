# Create a library with reusable test utilities.
add_library(
  wf_test_support
  INTERFACE
  wf_test_support/code_gen_helpers.h
  wf_test_support/eigen_test_macros.h
  wf_test_support/numeric_testing.h
  wf_test_support/test_macros.h
  wf_test_support/numerical_jacobian.h)

target_include_directories(
  wf_test_support INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                            $<INSTALL_INTERFACE:include>)

if(NOT TARGET wf_core)
  message(FATAL_ERROR "Main library target has not been defined.")
endif()

target_link_libraries(wf_test_support INTERFACE wf_runtime gtest eigen
                                                fmt::fmt-header-only)

# Object target for custom main() for gtest
set(CUSTOM_MAIN wf_custom_main)
add_library(${CUSTOM_MAIN} OBJECT custom_main.cc)
target_link_libraries(${CUSTOM_MAIN} gtest)
target_compile_features(${CUSTOM_MAIN} PRIVATE cxx_std_17)
target_compile_options(${CUSTOM_MAIN} PRIVATE ${SHARED_WARNING_FLAGS})
