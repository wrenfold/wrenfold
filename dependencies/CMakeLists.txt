function(add_libfmt)
  set(FMT_TEST
      OFF
      CACHE BOOL "" FORCE) # Turn off libfmt tests
  add_subdirectory(fmt EXCLUDE_FROM_ALL) # Don't need to install.
endfunction()
add_libfmt()

function(add_gtest)
  set(BUILD_GMOCK
      OFF
      CACHE BOOL "" FORCE)
  set(INSTALL_GTEST
      OFF
      CACHE BOOL "" FORCE)
  add_subdirectory(gtest)
endfunction()
add_gtest()

# Google benchmark
function(add_google_benchmark)
  set(BENCHMARK_ENABLE_TESTING
      OFF
      CACHE BOOL "" FORCE)
  set(BENCHMARK_ENABLE_INSTALL
      OFF
      CACHE BOOL "" FORCE)
  if(MSVC)
    # Turn off warning about failure to specify /EHs
    add_compile_options(/wd4530)
  endif()
  add_subdirectory(benchmark)
endfunction()
add_google_benchmark()

# Add abseil-cpp
function(add_abseil)
  set(ABSL_USE_EXTERNAL_GOOGLETEST
      ON
      CACHE BOOL "" FORCE)
  set(ABSL_ENABLE_INSTALL
      OFF
      CACHE BOOL "" FORCE)
  set(ABSL_PROPAGATE_CXX_STD
      ON
      CACHE BOOL "" FORCE)
  add_subdirectory(abseil-cpp)
endfunction()
add_abseil()

# Add pybind:
function(add_pybind)
  add_subdirectory(pybind11)
endfunction()
add_pybind()

# Create a target for eigen.
function(add_eigen)
  set(EIGEN_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/eigen")
  if(NOT TARGET eigen)
    add_library(eigen INTERFACE IMPORTED GLOBAL)
    set_target_properties(eigen PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
                                           "${EIGEN_INCLUDE_DIRS}")
  endif()
endfunction()
add_eigen()
