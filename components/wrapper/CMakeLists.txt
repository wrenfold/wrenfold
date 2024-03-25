add_subdirectory(pywrenfold)

# Add python tests:
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(tests)
endif()
