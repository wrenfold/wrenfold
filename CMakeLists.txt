cmake_minimum_required(VERSION 3.18)
project(
  zen-codegen
  VERSION 0.1
  DESCRIPTION "Tools for generating code from symbolic math."
  LANGUAGES CXX)

enable_testing()
include(GNUInstallDirs)
set(CMAKE_COLOR_MAKEFILE ON)

# Find python (for pybind11)
find_package(
  Python 3.8
  COMPONENTS Interpreter Development.Module
  REQUIRED)
message(STATUS "Python_INCLUDE_DIRS: ${Python_INCLUDE_DIRS}")

# Add third party code
add_subdirectory(dependencies)

set(PROJECT_PREFIX zen)
set(LIBRARY_NAME ${PROJECT_PREFIX}-math)

# Adjust warnings
if(MSVC)
  # We turn off C4702 (unreachable code) because constexpr blocks trigger it
  # incorrectly.
  set(MATH_COMPILATION_FLAGS
      /W4
      /WX
      /D_USE_MATH_DEFINES
      /bigobj
      /wd4244
      /wd4702
      /EHs
      /Zc:__cplusplus)
else()
  set(MATH_COMPILATION_FLAGS -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
  if(WIN32)
    list(APPEND MATH_COMPILATION_FLAGS -D_USE_MATH_DEFINES)
  endif()
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # Disable unused parameter warning (triggered by if constexpr)
  list(APPEND MATH_COMPILATION_FLAGS -Wno-unused-parameter
       -Wno-unused-but-set-parameter)
endif()

# Add libraries
add_subdirectory(components)

# Add tests, if not building from scikit-build-core
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(test)
  add_subdirectory(benchmarks)
endif()
