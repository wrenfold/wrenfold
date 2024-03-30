# Look for sphinx-build. If it is present, assume we can build docs.
find_program(SPHINX_BUILD_PATH sphinx-build)
if(DEFINED SPHINX_BUILD_PATH)
  message(STATUS "sphinx-build path: ${SPHINX_BUILD_PATH}")

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
  set(GENERATED_RST_FILES source/python_api/geometry.rst
                          source/python_api/sym.rst)

  set(HANDWRITTEN_RST_FILES source/python_api/index.rst)

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
    DEPENDS wf-core wf_wrapper wf_python ${PYTHON_LIB_SOURCES}
            ${DOC_GENERATION_SCRIPT} ${HANDWRITTEN_RST_FILES})

  # The handwritten source files for the docs:
  set(DOC_INPUT_FILES source/conf.py source/index.rst)

  # Add command that runs sphinx-build. This depends explicitly on
  # wf_wrapper_and_stubs so that the wrapper and stubs are up to date when docs
  # are generated. Specify `-E -a` so that this runs irrespective of whether
  # `.rst` files have changed.
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html"
    COMMAND ${SPHINX_BUILD_PATH} -E -a source build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generate documentation with sphinx."
    DEPENDS ${MODULE_NAME} wf_wrapper_and_stubs ${DOC_INPUT_FILES}
            ${GENERATED_RST_FILES} ${HANDWRITTEN_RST_FILES})
  add_custom_target(wf_docs
                    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html")
endif()
