# Create a target for the runtime library (a single header).
set(RUNTIME_INTERFACE_NAME math_runtime)
add_library(${RUNTIME_INTERFACE_NAME} INTERFACE)
target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

# Specify public headers
set(${RUNTIME_INTERFACE_NAME}_HEADERS span.h)
set_target_properties(
  ${RUNTIME_INTERFACE_NAME} PROPERTIES PUBLIC_HEADER
                                       "${RUNTIME_INTERFACE_NAME}_HEADERS")
