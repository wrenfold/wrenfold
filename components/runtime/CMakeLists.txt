# Create a target for the runtime library (a header only target).
set(RUNTIME_INTERFACE_NAME wf_runtime)
add_library(${RUNTIME_INTERFACE_NAME} INTERFACE wf_runtime/span.h
            wf_runtime/span_detail.h wf_runtime/span_eigen.h)
target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(tests)
endif()

if(DEFINED SKBUILD_PROJECT_NAME)
  # Setup install target for the runtime:
  install(
    TARGETS wf_runtime
    EXPORT wf_runtime
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/wf_runtime")
endif()
