# Create a target for the runtime library (a header only target).
set(RUNTIME_INTERFACE_NAME ${PROJECT_PREFIX}-runtime)

add_library(${RUNTIME_INTERFACE_NAME} INTERFACE)

target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

# Specify public headers set(${RUNTIME_INTERFACE_NAME}_HEADERS )
set_target_properties(
  ${RUNTIME_INTERFACE_NAME} PROPERTIES PUBLIC_HEADER span.h span_detail.h
                                                            span_eigen.h)

add_subdirectory(tests)
