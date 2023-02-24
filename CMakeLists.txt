cmake_minimum_required(VERSION 3.18)
project(
  mathcpp
  VERSION 0.1
  DESCRIPTION "Messing around with math in c++."
  LANGUAGES CXX)

enable_testing()
include(GNUInstallDirs)

set(CMAKE_COLOR_MAKEFILE ON)

# Find python (for nanobind)
find_package(
  Python 3.8
  COMPONENTS Interpreter Development.Module
  REQUIRED)
message(STATUS "Python_INCLUDE_DIRS: ${Python_INCLUDE_DIRS}")

# Add third party code
add_subdirectory(dependencies)

set(LIBRARY_NAME ${PROJECT_NAME})
set(${LIBRARY_NAME}_SOURCES
    source/constants.cc
    source/expression.cc
    source/expressions/addition.cc
    source/expressions/constant_expressions.cc
    source/expressions/matrix.cc
    source/expressions/multiplication.cc
    source/expressions/power.cc
    source/functions.cc
    source/hashing.cc
    source/plain_formatter.cc
    source/matrix_functions.cc
    source/tree_formatter.cc
    source/visitor.cc
    source/code_generation.cc)

# define binary from the sources
add_library(${LIBRARY_NAME} STATIC ${${LIBRARY_NAME}_SOURCES})

# Turn on C++17
target_compile_features(${LIBRARY_NAME} PUBLIC cxx_std_17)

# Set -fPIC
set_target_properties(${LIBRARY_NAME} PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Adjust warnings
if(MSVC)
  set(MATH_COMPILATION_FLAGS /W4 /WX /D_USE_MATH_DEFINES /bigobj /wd4244)
else()
  set(MATH_COMPILATION_FLAGS -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
endif()

target_compile_options(${LIBRARY_NAME} PRIVATE ${MATH_COMPILATION_FLAGS})
target_link_libraries(${LIBRARY_NAME} fmt)

# Includes
target_include_directories(
  ${LIBRARY_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                         $<INSTALL_INTERFACE:include>)

# Specify public headers
file(GLOB_RECURSE ${LIBRARY_NAME}_HEADERS "include/*.h*")
set_target_properties(${LIBRARY_NAME} PROPERTIES PUBLIC_HEADER
                                                 "${${LIBRARY_NAME}_HEADERS}")

# Setup install target
install(
  TARGETS ${LIBRARY_NAME}
  EXPORT ${LIBRARY_NAME}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  INCLUDES
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${LIBRARY_NAME}")

# Add tests
add_subdirectory(test)

# Add nanobind wrapper.
add_subdirectory(wrapper)
