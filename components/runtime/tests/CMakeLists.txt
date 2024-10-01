# Add test for span:
add_executable(span_test span_test.cc)
target_link_libraries(span_test gtest_main)
target_link_libraries(span_test wf_runtime eigen fmt::fmt-header-only)
target_compile_features(span_test PRIVATE cxx_std_17)
target_compile_options(span_test PRIVATE ${WF_SHARED_WARNING_FLAGS})

# Enable support for Eigen --> span conversion so we can test it.
target_compile_definitions(span_test PRIVATE -DWF_SPAN_EIGEN_SUPPORT)

add_test(span_test span_test)
