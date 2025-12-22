cmake_policy(SET CMP0077 NEW)
set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)

function(add_libfmt)
  set(FMT_TEST OFF)
  if(NOT TARGET fmt::fmt-header-only)
    add_subdirectory(fmt EXCLUDE_FROM_ALL)
  endif()
endfunction()
add_libfmt()

function(add_gtest)
  set(BUILD_GMOCK OFF)
  set(INSTALL_GTEST OFF)
  if(NOT TARGET gtest)
    add_subdirectory(gtest)
  endif()
endfunction()

if(WF_BUILD_EXTRAS)
  add_gtest()
endif()

# Google benchmark
function(add_google_benchmark)
  set(BENCHMARK_ENABLE_TESTING OFF)
  set(BENCHMARK_ENABLE_INSTALL OFF)
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
  # https://github.com/abseil/abseil-cpp/blob/master/CMake/README.md
  set(CMAKE_CXX_STANDARD 17)
  set(ABSL_USE_EXTERNAL_GOOGLETEST ON)
  set(ABSL_BUILD_TESTING OFF)
  set(ABSL_ENABLE_INSTALL OFF)
  set(ABSL_PROPAGATE_CXX_STD ON)
  if(NOT TARGET absl::span)
    add_subdirectory(abseil-cpp EXCLUDE_FROM_ALL)
  endif()
endfunction()
add_abseil()

# Add nanobind:
function(add_nanobind)
  add_subdirectory(nanobind)
endfunction()
add_nanobind()

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
  if(NOT TARGET Eigen3::Eigen)
    add_library(eigen INTERFACE IMPORTED GLOBAL)
    set_target_properties(eigen PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
                                           "${EIGEN_INCLUDE_DIRS}")
    add_library(Eigen3::Eigen ALIAS eigen)
  endif()
endfunction()
add_eigen()
