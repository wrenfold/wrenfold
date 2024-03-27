cmake_minimum_required(VERSION 3.20)
project(
  wrenfold-codegen
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
message(STATUS "Python_EXECUTABLE: ${Python_EXECUTABLE}")

# Add third party code
add_subdirectory(dependencies)

# Add our custom functions.
include(cmake/functions.cmake)

# Configure PYTHONPATH for scripts that depend on the python module + wrapper.
set_python_env_variables()

# Some shared warning flags:
if(MSVC)
  # We turn off C4702 (unreachable code) because constexpr blocks trigger it
  # incorrectly.
  set(SHARED_WARNING_FLAGS /W4 /WX /wd4244 /wd4702)
else()
  set(SHARED_WARNING_FLAGS -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # Disable unused parameter warning (triggered by if constexpr)
  list(APPEND SHARED_WARNING_FLAGS -Wno-unused-parameter
       -Wno-unused-but-set-parameter)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL
                                             "AppleClang")
  # Disable unused local typedef on Clang:
  list(APPEND SHARED_WARNING_FLAGS -Wno-unused-local-typedef)
endif()

# Custom target that groups together all the rust code-generation.
add_custom_target(wf_rust_generation ALL)

# Add libraries
add_subdirectory(components)

# Add examples:
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(examples)
  add_subdirectory(docs)
endif()
