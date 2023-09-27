# Find stubgen:
if(NOT DEFINED STUBGEN_PATH)
  message(STATUS "Looking for stubgen...")
  find_program(STUBGEN_PATH stubgen)
  if(DEFINED STUBGEN_PATH)
    message(STATUS "stubgen path: ${STUBGEN_PATH}")
  endif()
endif()

# Function for defining a wrapper module.
function(add_wrapper_module)
  set(oneValueArgs NAME)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(MODULE "" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Add pybind11 module
  pybind11_add_module(${MODULE_NAME} ${MODULE_SOURCES})

  # Depend on the static library
  add_dependencies(${MODULE_NAME} ${LIBRARY_NAME})
  target_link_libraries(${MODULE_NAME} PRIVATE ${LIBRARY_NAME})
  target_compile_options(${MODULE_NAME} PRIVATE ${MATH_COMPILATION_FLAGS})
  target_compile_definitions(${MODULE_NAME}
                             PRIVATE PY_MODULE_NAME=${MODULE_NAME})

  # Copy from Debug or Release into the current binary dir. We need to do this
  # because msbuild puts libraries in sub-directories, which breaks our ability
  # to import them from the cmake binary dir.
  if(${CMAKE_GENERATOR} MATCHES "Visual Studio.+")
    add_custom_command(
      TARGET ${MODULE_NAME}
      POST_BUILD
      COMMAND
        ${CMAKE_COMMAND} -E copy
        "$<TARGET_FILE_DIR:${MODULE_NAME}>/$<TARGET_FILE_NAME:${MODULE_NAME}>"
        "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_FILE_NAME:${MODULE_NAME}>"
      COMMENT "Copying module to binary dir: ${MODULE_NAME}")
  endif()

  # Add a command to generate stubs
  # https://mypy.readthedocs.io/en/stable/stubgen.html
  set(STUB_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/${MODULE_NAME}.pyi")
  if(DEFINED STUBGEN_PATH)
    message(STATUS "Will generate stubs for: ${MODULE_NAME}")
    add_custom_command(
      OUTPUT ${STUB_OUTPUT_PATH}
      COMMAND ${STUBGEN_PATH} -m ${MODULE_NAME} --output
              ${CMAKE_CURRENT_BINARY_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generate stubs for pybind module: ${MODULE_NAME}"
      DEPENDS ${MODULE_NAME})

    # Create a target labeled `ALL` so that the command runs:
    add_custom_target(${MODULE_NAME}_stubs ALL DEPENDS ${STUB_OUTPUT_PATH})
  endif()

  # Add install targets for scikit-build-core
  if(DEFINED SKBUILD_PROJECT_NAME)
    install(TARGETS ${MODULE_NAME} DESTINATION sym_wrapper)
    if(DEFINED STUBGEN_PATH)
      install(FILES ${STUB_OUTPUT_PATH} DESTINATION sym_wrapper)
    endif()
  endif()
endfunction()

add_wrapper_module(NAME ${PROJECT_PREFIX}_pysym SOURCES expression_wrapper.cc
                   matrix_wrapper.cc)
add_wrapper_module(NAME ${PROJECT_PREFIX}_pycodegen SOURCES codegen_wrapper.cc)

# Add python tests:
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(python_test)
endif()
