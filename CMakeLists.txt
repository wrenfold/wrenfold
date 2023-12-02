cmake_minimum_required(VERSION 3.19)
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

set(PROJECT_PREFIX wf)
set(LIBRARY_NAME ${PROJECT_PREFIX}-core)

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

# Add libraries
add_subdirectory(components)
