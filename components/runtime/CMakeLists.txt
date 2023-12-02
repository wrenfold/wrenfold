# Create a target for the runtime library (a header only target).
set(RUNTIME_INTERFACE_NAME ${PROJECT_PREFIX}-runtime)
add_library(${RUNTIME_INTERFACE_NAME} INTERFACE wf_runtime/span.h
            wf_runtime/span_detail.h wf_runtime/span_eigen.h)
target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

add_subdirectory(tests)
