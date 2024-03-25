# Look for sphinx-build. If it is present, assume we can build docs.
find_program(SPHINX_BUILD_PATH sphinx-build)
if(DEFINED SPHINX_BUILD_PATH)
  message(STATUS "sphinx-build path: ${SPHINX_BUILD_PATH}")

  set(DOC_INPUT_FILES source/conf.py source/index.rst
                      source/python_api_docs.rst)

  # Add command that runs sphinx-build. This depends explicitly on
  # wf_wrapper_and_stubs so that the wrapper and stubs are up to date when docs
  # are generated. Specify `-E -a` so that this runs irrespective of whether
  # `.rst` files have changed.
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html"
    COMMAND ${SPHINX_BUILD_PATH} -E -a source build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generate documentation with sphinx."
    DEPENDS ${MODULE_NAME} wf_wrapper_and_stubs ${DOC_INPUT_FILES})
  add_custom_target(wf_docs
                    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/build/index.html")
endif()
