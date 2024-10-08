function(add_libfmt)
  set(FMT_TEST
      OFF
      CACHE BOOL "" FORCE) # Turn off libfmt tests
  if(NOT TARGET fmt::fmt-header-only)
    add_subdirectory(fmt EXCLUDE_FROM_ALL) # Don't need to install.
  endif()
endfunction()
add_libfmt()

function(add_gtest)
  set(BUILD_GMOCK
      OFF
      CACHE BOOL "" FORCE)
  set(INSTALL_GTEST
      OFF
      CACHE BOOL "" FORCE)
  if(NOT TARGET gtest)
    add_subdirectory(gtest)
  endif()
endfunction()

if(WF_BUILD_EXTRAS)
  add_gtest()
endif()

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
  if(NOT TARGET benchmark::benchmark)
    add_subdirectory(benchmark)
  endif()
endfunction()

if(WF_BUILD_EXTRAS)
  add_google_benchmark()
endif()

# Add abseil-cpp
function(add_abseil)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
  # wf-core builds with c++17
  # https://github.com/abseil/abseil-cpp/blob/master/CMake/README.md
  set(CMAKE_CXX_STANDARD 17)
  set(ABSL_USE_EXTERNAL_GOOGLETEST
      ON
      CACHE BOOL "" FORCE)
  set(ABSL_BUILD_TESTING
      OFF
      CACHE BOOL "" FORCE)
  set(ABSL_ENABLE_INSTALL
      OFF
      CACHE BOOL "" FORCE)
  set(ABSL_PROPAGATE_CXX_STD
      ON
      CACHE BOOL "" FORCE)
  if(NOT TARGET absl::span)
    add_subdirectory(abseil-cpp EXCLUDE_FROM_ALL)
  endif()
endfunction()
add_abseil()

# Add pybind:
function(add_pybind)
  if(NOT TARGET pybind11::module)
    add_subdirectory(pybind11)
  endif()
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
