cmake_minimum_required(VERSION 3.18)
project(math_cpp VERSION 0.1
                 DESCRIPTION "Messing around with math."
                 LANGUAGES CXX)
enable_testing()
include(FetchContent)

set(CMAKE_COLOR_MAKEFILE ON)
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  ${PROJECT_SOURCE_DIR}/cmake
)

# Get libfmt
FetchContent_Declare(
    libfmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 8.0.1
)
FetchContent_GetProperties(libfmt)
if (NOT libfmt_POPULATED)
    FetchContent_Populate(libfmt)
    add_subdirectory(${libfmt_SOURCE_DIR} ${libfmt_BINARY_DIR})
endif ()

# Find all the source files.
set(EXECUTABLE_NAME "${PROJECT_NAME}")
FILE(GLOB ${EXECUTABLE_NAME}_SOURCES "source/*.cc")

# define binary from the sources
add_executable(${EXECUTABLE_NAME} ${${EXECUTABLE_NAME}_SOURCES})

# Turn on C++14
target_compile_features(${EXECUTABLE_NAME} PRIVATE cxx_std_14)

# turn on various warnings
if (MSVC)
    # USE_MATH_DEFINES is required for M_PI
    target_compile_options(${EXECUTABLE_NAME} PRIVATE /W4 /WX /D_USE_MATH_DEFINES /bigobj /wd4244)
else ()
    target_compile_options(${EXECUTABLE_NAME} PRIVATE
            -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
endif ()

# Link
target_link_libraries(${EXECUTABLE_NAME} fmt)

# Includes
target_include_directories(${EXECUTABLE_NAME} 
  PUBLIC 
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
