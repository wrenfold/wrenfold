# Add test for span:
add_executable(span_test span_test.cc)
target_link_libraries(span_test gtest_main)
target_link_libraries(span_test wf-runtime eigen fmt::fmt-header-only)
target_compile_features(span_test PRIVATE cxx_std_14)
target_compile_options(span_test PRIVATE ${SHARED_WARNING_FLAGS})
target_compile_definitions(span_test PRIVATE -DMATH_SPAN_EIGEN_SUPPORT)
add_test(span_test span_test)
