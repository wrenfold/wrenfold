# Define a property for the list of submodules in our top level C++ module.
set(PYTHON_SUBMODULES_DOC
    "List of submodules built into the C++ wrapper module.")
define_property(
  TARGET
  PROPERTY PYTHON_SUBMODULES
  BRIEF_DOCS ${PYTHON_SUBMODULES_DOC}
  FULL_DOCS ${PYTHON_SUBMODULES_DOC})

if(WF_BUILD_STUBS)
  add_custom_target(wf_stubs ALL)
endif()

# Define a target for the C++ wrapper module.
function(add_wrapper_module)
  set(oneValueArgs NAME)
  set(multiValueArgs "SOURCES;SUBMODULE_NAMES")
  cmake_parse_arguments(MODULE "" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Add pybind11 module
  pybind11_add_module(${MODULE_NAME} ${MODULE_SOURCES})
  set_property(
    TARGET ${MODULE_NAME}
    APPEND
    PROPERTY PYTHON_SUBMODULES ${MODULE_SUBMODULE_NAMES})

  # Depend on the static library
  target_link_libraries(${MODULE_NAME} PRIVATE wf_core)
  target_compile_options(${MODULE_NAME} PRIVATE ${WF_SHARED_WARNING_FLAGS})
  target_compile_options(${MODULE_NAME}
                         PRIVATE -DPYBIND11_DETAILED_ERROR_MESSAGES)
  target_compile_definitions(${MODULE_NAME}
                             PRIVATE PY_MODULE_NAME=${MODULE_NAME})
  if(DEFINED SKBUILD_PROJECT_VERSION)
    target_compile_definitions(
      ${MODULE_NAME} PRIVATE PY_MODULE_VERSION="${SKBUILD_PROJECT_VERSION}")
  else()
    target_compile_definitions(${MODULE_NAME} PRIVATE PY_MODULE_VERSION="dev")
  endif()

  find_package(Git)
  if(Git_FOUND)
    execute_process(
      COMMAND ${GIT_EXECUTABLE} log -1 --format=%H
      WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
      OUTPUT_VARIABLE GIT_HASH
      OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE GIT_HASH_RESULT_VAR)
    if(GIT_HASH_RESULT_VAR EQUAL "0")
      message(STATUS "git hash: ${GIT_HASH}")
      target_compile_definitions(${MODULE_NAME}
                                 PRIVATE PY_GIT_HASH="${GIT_HASH}")
    else()
      message(STATUS "Failed to obtain git hash.")
    endif()
  endif()

  # For each submodule, define a compile-time constant for its name.
  foreach(SUBMODULE_NAME ${MODULE_SUBMODULE_NAMES})
    string(TOUPPER ${SUBMODULE_NAME} SUBMODULE_NAME_UPPER)
    target_compile_definitions(
      ${MODULE_NAME}
      PRIVATE "PY_SUBMODULE_NAME_${SUBMODULE_NAME_UPPER}=\"${SUBMODULE_NAME}\"")
  endforeach()

  # Add install targets for scikit-build-core
  if(DEFINED SKBUILD_PROJECT_NAME)
    if(NOT DEFINED SKBUILD_PLATLIB_DIR)
      message(FATAL_ERROR "Expected SKBUILD_PLATLIB_DIR to be defined.")
    endif()
    install(TARGETS ${MODULE_NAME} DESTINATION ${SKBUILD_PLATLIB_DIR})
  endif()
endfunction()

# Executes mypy-stubgen to generate stubs for the C++ wrapper module.
function(add_wrapper_module_stubs)
  set(oneValueArgs MODULE_NAME OUTPUT_DIR)
  set(multiValueArgs "")
  cmake_parse_arguments(ARGS "" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Find stubgen:
  if(NOT DEFINED STUBGEN_PATH OR STUBGEN_PATH STREQUAL "STUBGEN_PATH-NOTFOUND")
    message(STATUS "Looking for stubgen...")
    find_program(STUBGEN_PATH stubgen REQUIRED)
    if(DEFINED STUBGEN_PATH AND NOT STUBGEN_PATH STREQUAL
                                "STUBGEN_PATH-NOTFOUND")
      message(STATUS "stubgen path: ${STUBGEN_PATH}")
    endif()
  endif()

  # Get the sub-module names from the module target.
  get_property(
    SUBMODULE_NAMES
    TARGET ${ARGS_MODULE_NAME}
    PROPERTY PYTHON_SUBMODULES)

  # There can be multiple submodules in the same shared object. Iterate over all
  # of them and make sure we record their output `pyi` files.
  set(STUBGEN_BUILD_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
  set(STUBGEN_BUILD_OUTPUT_PATHS
      "${STUBGEN_BUILD_OUTPUT_DIR}/${ARGS_MODULE_NAME}/__init__.pyi")
  foreach(SUBMODULE_NAME ${SUBMODULE_NAMES})
    set(SUBMODULE_STUB_OUTPUT_PATH
        "${STUBGEN_BUILD_OUTPUT_DIR}/${ARGS_MODULE_NAME}/${SUBMODULE_NAME}.pyi")
    list(APPEND STUBGEN_BUILD_OUTPUT_PATHS ${SUBMODULE_STUB_OUTPUT_PATH})
  endforeach()

  # The directory in the source tree we will copy these to.
  set(STUBGEN_SOURCE_OUTPUT_DIR ${ARGS_OUTPUT_DIR})
  set(STUBGEN_SOURCE_OUTPUT_PATHS "")
  foreach(BUILD_PATH ${STUBGEN_BUILD_OUTPUT_PATHS})
    get_filename_component(STUB_NAME ${BUILD_PATH} NAME)
    list(APPEND STUBGEN_SOURCE_OUTPUT_PATHS
         "${STUBGEN_SOURCE_OUTPUT_DIR}/${ARGS_MODULE_NAME}/${STUB_NAME}")
  endforeach()

  # Add a command to generate stubs
  # https://mypy.readthedocs.io/en/stable/stubgen.html
  message(STATUS "Will generate stubs for: ${ARGS_MODULE_NAME}")
  add_custom_command(
    OUTPUT ${STUBGEN_BUILD_OUTPUT_PATHS}
    COMMAND ${STUBGEN_PATH} --package ${ARGS_MODULE_NAME} --output
            ${STUBGEN_BUILD_OUTPUT_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generate stubs for pybind module: ${ARGS_MODULE_NAME}"
    DEPENDS ${ARGS_MODULE_NAME})

  # Copy stubs to the source tree:
  add_custom_command(
    OUTPUT ${STUBGEN_SOURCE_OUTPUT_PATHS}
    COMMAND ${CMAKE_COMMAND} -E copy ${STUBGEN_BUILD_OUTPUT_PATHS}
            "${STUBGEN_SOURCE_OUTPUT_DIR}/${ARGS_MODULE_NAME}"
    DEPENDS ${STUBGEN_BUILD_OUTPUT_PATHS}
    COMMENT "Copying stubs to source tree.")

  add_custom_target(${ARGS_MODULE_NAME}_stubs
                    DEPENDS ${STUBGEN_SOURCE_OUTPUT_PATHS})
  add_dependencies(wf_stubs ${ARGS_MODULE_NAME}_stubs)
endfunction()

# Add install targets for generated stubs.
function(install_stubs)
  set(oneValueArgs MODULE_NAME STUBS_DIR)
  set(multiValueArgs "")
  cmake_parse_arguments(ARGS "" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Get the sub-module names from the module target. There is one stub for each
  # of these.
  get_property(
    SUBMODULE_NAMES
    TARGET ${ARGS_MODULE_NAME}
    PROPERTY PYTHON_SUBMODULES)
  message(STATUS "Will install stubs for modules: ${SUBMODULE_NAMES}")

  # The same logic appears above in `add_wrapper_module_stubs`. We determine the
  # paths for all stubs that need to be installed.
  set(STUBGEN_OUTPUT_DIR ${ARGS_STUBS_DIR})
  set(STUBGEN_OUTPUT_PATHS
      "${STUBGEN_OUTPUT_DIR}/${ARGS_MODULE_NAME}/__init__.pyi")
  foreach(SUBMODULE_NAME ${SUBMODULE_NAMES})
    set(SUBMODULE_STUB_OUTPUT_PATH
        "${STUBGEN_OUTPUT_DIR}/${ARGS_MODULE_NAME}/${SUBMODULE_NAME}.pyi")
    list(APPEND STUBGEN_OUTPUT_PATHS ${SUBMODULE_STUB_OUTPUT_PATH})
  endforeach()

  # Add install targets for scikit-build-core.
  if(DEFINED SKBUILD_PROJECT_NAME)
    if(NOT DEFINED SKBUILD_PLATLIB_DIR)
      message(FATAL_ERROR "Expected SKBUILD_PLATLIB_DIR to be defined.")
    endif()
    install(FILES ${STUBGEN_OUTPUT_PATHS}
            DESTINATION "${SKBUILD_PLATLIB_DIR}/${ARGS_MODULE_NAME}")
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
  pywrenfold/docs/expressions_wrapper.h
  pywrenfold/docs/geometry_wrapper.h
  pywrenfold/docs/matrix_wrapper.h
  pywrenfold/docs/scalar_wrapper.h
  pywrenfold/docs/symbolic_functions_wrapper.h
  pywrenfold/docs/sympy_conversion.h
  pywrenfold/enums_wrapper.cc
  pywrenfold/exceptions_wrapper.cc
  pywrenfold/expressions_wrapper.cc
  pywrenfold/geometry_wrapper.cc
  pywrenfold/matrix_wrapper.cc
  pywrenfold/scalar_wrapper.cc
  pywrenfold/symbolic_functions_wrapper.cc
  pywrenfold/sympy_conversion.cc
  pywrenfold/types_wrapper.cc
  pywrenfold/visitor_wrappers.h
  pywrenfold/wrapper.cc
  pywrenfold/wrapper_utils.h
  SUBMODULE_NAMES
  ast
  enumerations
  exceptions
  expressions
  gen
  geometry
  sym
  sympy_conversion
  type_info)

add_library(wf_wrapper ALIAS pywrenfold)

# Because we can't build stubs while cross-compiling (necessary for Conda), we
# instead build them to the repository check that they have been committed
# properly on CI.
if(WF_BUILD_STUBS)
  add_wrapper_module_stubs(MODULE_NAME pywrenfold OUTPUT_DIR
                           "${CMAKE_CURRENT_SOURCE_DIR}/stubs")
endif()

# Stubs will be installed from their location in the repo.
install_stubs(MODULE_NAME pywrenfold STUBS_DIR
              "${CMAKE_CURRENT_SOURCE_DIR}/stubs")

# Add python tests:
if(WF_BUILD_EXTRAS)
  add_subdirectory(tests)
endif()
