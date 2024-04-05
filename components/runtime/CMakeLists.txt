# Create a target for the runtime library (a header only target).
set(RUNTIME_INTERFACE_NAME wf_runtime)
set(RUNTIME_SOURCES wrenfold/span.h wrenfold/span_detail.h
                    wrenfold/span_eigen.h)
add_library(${RUNTIME_INTERFACE_NAME} INTERFACE ${RUNTIME_SOURCES})
target_include_directories(
  ${RUNTIME_INTERFACE_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include>)

if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(tests)
endif()

if(DEFINED SKBUILD_PROJECT_NAME)
  # This will install the headers under: `include/site/pythonX.Y/wrenfold`.
  install(FILES ${RUNTIME_SOURCES} DESTINATION "${SKBUILD_HEADERS_DIR}")
else()
  install(FILES ${RUNTIME_SOURCES}
          DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/wrenfold")
endif()
