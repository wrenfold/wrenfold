set(FMT_TEST
    OFF
    CACHE BOOL "" FORCE) # Turn off libfmt tests
add_subdirectory(fmt EXCLUDE_FROM_ALL) # Don't need to install.

set(BUILD_GMOCK
    OFF
    CACHE BOOL "" FORCE)
set(INSTALL_GTEST
    OFF
    CACHE BOOL "" FORCE)
add_subdirectory(gtest)

# Add nanobind:
add_subdirectory(nanobind)