cmake_minimum_required(VERSION 3.20)

if(DEFINED SKBUILD_PROJECT_NAME AND DEFINED SKBUILD_PROJECT_VERSION)
  project(
    ${SKBUILD_PROJECT_NAME}
    VERSION ${SKBUILD_PROJECT_VERSION}
    DESCRIPTION "Tools for generating code from symbolic math."
    LANGUAGES CXX C)
  message(STATUS "Building project version: ${SKBUILD_PROJECT_VERSION}")
  set(WF_BUILD_EXTRAS_DEFAULT OFF)
else()
  project(wrenfold CXX C)
  set(WF_BUILD_EXTRAS_DEFAULT ON)
endif()

enable_testing()
include(GNUInstallDirs)

# Find python (for pybind11)
find_package(
  Python 3.9
  COMPONENTS Interpreter Development.Module
  REQUIRED)
message(STATUS "Python_INCLUDE_DIRS: ${Python_INCLUDE_DIRS}")
message(STATUS "Python_EXECUTABLE: ${Python_EXECUTABLE}")

option(WF_BUILD_EXTRAS "Build tests, benchmarks, examples, and docs."
       ${WF_BUILD_EXTRAS_DEFAULT})
option(WF_STUBS_REQUIRED "Fail if stubgen cannot be found." OFF)
option(WF_DOCS_REQUIRED "Fail if sphinx/doxygen cannot be found." OFF)

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
  set(WF_SHARED_WARNING_FLAGS /W4 /WX /wd4702)
else()
  set(WF_SHARED_WARNING_FLAGS -Wall -Wextra -pedantic -Werror)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # Disable unused parameter warning (triggered by if constexpr)
  list(APPEND WF_SHARED_WARNING_FLAGS -Wno-unused-parameter
       -Wno-unused-but-set-parameter)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL
                                             "AppleClang")
  # Disable unused local typedef on Clang:
  list(APPEND WF_SHARED_WARNING_FLAGS -Wno-unused-local-typedef)
endif()

# Custom target that groups together all the rust code-generation.
add_custom_target(wf_rust_generation ALL)

# Add libraries
add_subdirectory(components)

# Add examples and docs.
if(WF_BUILD_EXTRAS)
  add_subdirectory(examples)
  add_subdirectory(docs)
endif()
