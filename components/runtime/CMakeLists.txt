# Create a target for the runtime library (a header only target).
set(RUNTIME_INTERFACE_NAME wf_runtime)
set(RUNTIME_SOURCES wrenfold/span.h wrenfold/span_detail.h)
add_library(${RUNTIME_INTERFACE_NAME} INTERFACE ${RUNTIME_SOURCES})
target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

if(WF_BUILD_EXTRAS)
  add_subdirectory(tests)
endif()

if("$ENV{WF_SKIP_HEADER_INSTALL}" STREQUAL "skip")
  message(
    STATUS "WF_SKIP_HEADER_INSTALL specified, skipping header installation.")
else()
  if(DEFINED SKBUILD_HEADERS_DIR)
    # This will install the headers under: `include/site/pythonX.Y/wrenfold`.
    message(STATUS "Installing headers to: ${SKBUILD_HEADERS_DIR}")
    install(FILES ${RUNTIME_SOURCES} DESTINATION "${SKBUILD_HEADERS_DIR}")
    install(FILES ${CMAKE_SOURCE_DIR}/LICENSE
            DESTINATION "${SKBUILD_HEADERS_DIR}")
  else()
    # This will install headers to the normal include directory.
    message(STATUS "Installing headers to: ${CMAKE_INSTALL_INCLUDEDIR}")
    install(FILES ${RUNTIME_SOURCES}
            DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/wrenfold")
    install(FILES ${CMAKE_SOURCE_DIR}/LICENSE
            DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/wrenfold")
  endif()
endif()
