function(add_libfmt)
  set(FMT_TEST
      OFF
      CACHE BOOL "" FORCE) # Turn off libfmt tests
  add_subdirectory(fmt EXCLUDE_FROM_ALL) # Don't need to install.
  set_target_properties(fmt PROPERTIES POSITION_INDEPENDENT_CODE ON)
endfunction()
add_libfmt()

set(BUILD_GMOCK
    OFF
    CACHE BOOL "" FORCE)
set(INSTALL_GTEST
    OFF
    CACHE BOOL "" FORCE)
add_subdirectory(gtest)

# Add pybind:
add_subdirectory(pybind11)

# Create a target for eigen.
set(EIGEN_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/eigen")
if(NOT TARGET eigen)
  add_library(eigen INTERFACE IMPORTED GLOBAL)
  set_target_properties(eigen PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
                                         "${EIGEN_INCLUDE_DIRS}")
endif()
