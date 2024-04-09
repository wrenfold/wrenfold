# Look for sphinx-build. If it is present, assume we can build docs.
find_program(SPHINX_BUILD_PATH sphinx-build)
if(DEFINED SPHINX_BUILD_PATH AND NOT SPHINX_BUILD_PATH STREQUAL
                                 "SPHINX_BUILD_PATH-NOTFOUND")
  message(STATUS "sphinx-build path: ${SPHINX_BUILD_PATH}")
  set(SPHINX_FOUND TRUE)
else()
  set(SPHINX_FOUND FALSE)
endif()

# Look for doxygen.
find_program(DOXYGEN_PATH doxygen)
if(DEFINED DOXYGEN_PATH AND NOT DOXYGEN_PATH STREQUAL "DOXYGEN_PATH-NOTFOUND")
  message(STATUS "doxygen path: ${DOXYGEN_PATH}")
  set(DOXYGEN_FOUND TRUE)
else()
  set(DOXYGEN_FOUND FALSE)
endif()

# Doxygen workflow based on:
# https://devblogs.microsoft.com/cppblog/clear-functional-c-documentation-with-sphinx-breathe-doxygen-cmake/
if(${DOXYGEN_FOUND})
  # If doxygen is present, run it on the C++ runtime.
  set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
  set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  # Locate the runtime source directory:
  get_target_property(CPP_RUNTIME_SOURCE_DIR wf_runtime SOURCE_DIR)

  # Write doxygen docs into the build directory in docs/doxygen:
  set(DOXYGEN_INPUT_DIR ${CPP_RUNTIME_SOURCE_DIR}/wrenfold)
  set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/doxygen)
  configure_file(${DOXYFILE_IN} ${DOXYFILE_OUT} @ONLY)

  # Use main index.xml as the output file to track dependencies.
  set(DOXYGEN_INDEX_FILE ${DOXYGEN_OUTPUT_DIR}/xml/index.xml)

  file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})
  add_custom_command(
    OUTPUT ${DOXYGEN_INDEX_FILE}
    DEPENDS wf_runtime ${DOXYFILE_IN}
    COMMAND ${DOXYGEN_PATH} ${DOXYFILE_OUT}
    MAIN_DEPENDENCY ${DOXYFILE_OUT}
    COMMENT "Generating doxygen docs for runtime.")

  add_custom_target(wf_runtime_doxygen DEPENDS ${DOXYGEN_INDEX_FILE})
endif()

if(${SPHINX_FOUND})
  if(NOT DEFINED Python_EXECUTABLE)
    message(FATAL_ERROR "The Python executable could not be located.")
  endif()
  if(NOT DEFINED PYTHON_COMMAND_ENV_VARIABLES)
    message(
      FATAL_ERROR "Variable PYTHON_COMMAND_ENV_VARIABLES must be defined.")
  endif()

  # Get a list of python sources in `wf_python`.
  get_python_library_sources(PYTHON_LIB_SOURCES)

  # The python script we use to generate RST files, and the list of output
  # files:
  set(DOC_GENERATION_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/generate_api_rst.py")
  set(GENERATED_RST_FILES
      source/python_api/ast.rst
      source/python_api/code_generation.rst
      source/python_api/enumerations.rst
      source/python_api/exceptions.rst
      source/python_api/geometry.rst
      source/python_api/sym.rst
      source/python_api/sympy_conversion.rst
      source/python_api/type_annotations.rst
      source/python_api/type_info.rst)

  set(HANDWRITTEN_RST_FILES
      source/index.rst
      source/tutorial/conditionals.rst
      source/tutorial/custom_types.rst
      source/tutorial/custom_types_script.py
      source/tutorial/external_functions.rst
      source/tutorial/external_functions_script.py
      source/tutorial/generating_code.rst
      source/tutorial/index.rst
      source/tutorial/installation.rst
      source/tutorial/integrating_code.rst
      source/tutorial/introduction.rst
      source/tutorial/matrices.rst
      source/tutorial/new_language.rst
      source/tutorial/symbolic_manipulation.rst
      source/tutorial/sympy_interop.rst
      source/python_api/index.rst
      source/cpp_api/index.rst
      source/cpp_api/span.rst)

  list(TRANSFORM GENERATED_RST_FILES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
  list(TRANSFORM HANDWRITTEN_RST_FILES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")

  # Invoke `generate_api_rst.py` to generate RST files for our python modules.
  add_custom_command(
    OUTPUT ${GENERATED_RST_FILES}
    COMMAND
      ${CMAKE_COMMAND} -E env ${PYTHON_COMMAND_ENV_VARIABLES}
      ${Python_EXECUTABLE} -B ${DOC_GENERATION_SCRIPT}
      "${CMAKE_CURRENT_SOURCE_DIR}/source/python_api"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generate RST for python API."
    DEPENDS wf_core wf_wrapper wf_python ${PYTHON_LIB_SOURCES}
            ${DOC_GENERATION_SCRIPT} ${HANDWRITTEN_RST_FILES})

  # Add command that runs sphinx-build. This depends explicitly on wf_stubs so
  # that the wrapper and stubs are up to date when docs are generated. Specify
  # `-E -a` so that this runs irrespective of whether `.rst` files have changed.
  set(SPHINX_ARGS -E -a source build)

  if(DEFINED DOXYGEN_OUTPUT_DIR)
    message(STATUS "Configuring sphinx to ingest doxygen output.")

    # Tell sphinx where to find the doxygen output:
    list(APPEND SPHINX_ARGS
         -Dbreathe_projects.wf_runtime="${DOXYGEN_OUTPUT_DIR}/xml")
  endif()

  # Run sphinx through cmake so we can specify the python path correctly.
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html"
    COMMAND ${CMAKE_COMMAND} -E env ${PYTHON_COMMAND_ENV_VARIABLES}
            ${SPHINX_BUILD_PATH} ${SPHINX_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generate documentation with sphinx."
    DEPENDS wf_stubs source/conf.py ${GENERATED_RST_FILES}
            ${HANDWRITTEN_RST_FILES} ${DOXYGEN_INDEX_FILE})
  add_custom_target(wf_docs
                    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html")
endif()
