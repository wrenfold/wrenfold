# Custom target for all stubs:
add_custom_target(wf_stubs ALL)

# Find stubgen:
if(NOT DEFINED STUBGEN_PATH)
  message(STATUS "Looking for stubgen...")
  find_program(STUBGEN_PATH stubgen)
  if(DEFINED STUBGEN_PATH AND NOT STUBGEN_PATH STREQUAL "STUBGEN_PATH-NOTFOUND")
    message(STATUS "stubgen path: ${STUBGEN_PATH}")
  endif()
endif()

# Function for defining the wrapper module.
function(add_wrapper_module)
  set(oneValueArgs NAME)
  set(multiValueArgs "SOURCES;SUB_MODULE_NAMES")
  cmake_parse_arguments(MODULE "" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Add pybind11 module
  pybind11_add_module(${MODULE_NAME} ${MODULE_SOURCES})

  # Depend on the static library
  target_link_libraries(${MODULE_NAME} PRIVATE wf_core)
  target_compile_options(${MODULE_NAME} PRIVATE ${SHARED_WARNING_FLAGS})
  target_compile_definitions(${MODULE_NAME}
                             PRIVATE PY_MODULE_NAME=${MODULE_NAME})

  # For each submodule, define a compile-time constant for its name.
  foreach(SUB_MODULE_NAME ${MODULE_SUB_MODULE_NAMES})
    string(TOUPPER ${SUB_MODULE_NAME} SUB_MODULE_NAME_UPPER)
    target_compile_definitions(
      ${MODULE_NAME}
      PRIVATE
        "PY_SUBMODULE_NAME_${SUB_MODULE_NAME_UPPER}=\"${SUB_MODULE_NAME}\"")
  endforeach()

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

  # There can be multiple submodules in the same shared object. Iterate over all
  # of them and make sure we record their output `pyi` files.
  set(STUBGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
  set(STUBGEN_OUTPUT_PATHS "${STUBGEN_OUTPUT_DIR}/${MODULE_NAME}/__init__.pyi")
  foreach(SUB_MODULE_NAME ${MODULE_SUB_MODULE_NAMES})
    set(SUB_MODULE_STUB_OUTPUT_PATH
        "${STUBGEN_OUTPUT_DIR}/${MODULE_NAME}/${SUB_MODULE_NAME}.pyi")
    list(APPEND STUBGEN_OUTPUT_PATHS ${SUB_MODULE_STUB_OUTPUT_PATH})
  endforeach()

  # Add a command to generate stubs
  # https://mypy.readthedocs.io/en/stable/stubgen.html
  if(DEFINED STUBGEN_PATH AND NOT STUBGEN_PATH STREQUAL "STUBGEN_PATH-NOTFOUND")
    message(STATUS "Will generate stubs for: ${MODULE_NAME}")
    add_custom_command(
      OUTPUT ${STUBGEN_OUTPUT_PATHS}
      COMMAND ${STUBGEN_PATH} --package ${MODULE_NAME} --output
              ${STUBGEN_OUTPUT_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generate stubs for pybind module: ${MODULE_NAME}"
      DEPENDS ${MODULE_NAME})

    add_custom_target(${MODULE_NAME}_stubs ALL DEPENDS ${STUBGEN_OUTPUT_PATHS})
    add_dependencies(wf_stubs ${MODULE_NAME}_stubs)
  endif()

  # Add install targets for scikit-build-core
  if(DEFINED SKBUILD_PROJECT_NAME)
    if(NOT DEFINED SKBUILD_PLATLIB_DIR)
      message(FATAL_ERROR "Expected SKBUILD_PLATLIB_DIR to be defined.")
    endif()
    install(TARGETS ${MODULE_NAME} DESTINATION ${SKBUILD_PLATLIB_DIR})
    if(DEFINED STUBGEN_PATH)
      install(FILES ${STUB_FINAL_OUTPUT_PATHS}
              DESTINATION "${SKBUILD_PLATLIB_DIR}/${MODULE_NAME}")
    endif()
  endif()
endfunction()

add_wrapper_module(
  NAME
  pywrenfold
  SOURCES
  pywrenfold/args_visitor.cc
  pywrenfold/args_visitor.h
  pywrenfold/ast_wrapper.cc
  pywrenfold/boolean_expression_wrapper.cc
  pywrenfold/code_formatting_wrapper.cc
  pywrenfold/codegen_wrapper.cc
  pywrenfold/compound_expression_wrapper.cc
  pywrenfold/docs/codegen_wrapper.h
  pywrenfold/docs/compound_expression_wrapper.h
  pywrenfold/docs/geometry_wrapper.h
  pywrenfold/docs/matrix_wrapper.h
  pywrenfold/docs/scalar_wrapper.h
  pywrenfold/docs/sympy_conversion.h
  pywrenfold/enums_wrapper.cc
  pywrenfold/exceptions_wrapper.cc
  pywrenfold/geometry_wrapper.cc
  pywrenfold/matrix_wrapper.cc
  pywrenfold/scalar_wrapper.cc
  pywrenfold/sympy_conversion.cc
  pywrenfold/types_wrapper.cc
  pywrenfold/wrapper.cc
  pywrenfold/wrapper_utils.h
  SUB_MODULE_NAMES
  ast
  enumerations
  exceptions
  gen
  geometry
  sym
  sympy_conversion
  type_info)

add_library(wf_wrapper ALIAS pywrenfold)

# Add python tests:
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(tests)
endif()
