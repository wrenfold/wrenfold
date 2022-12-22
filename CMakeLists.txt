cmake_minimum_required(VERSION 3.18)
project(
  math_cpp
  VERSION 0.1
  DESCRIPTION "Messing around with math in c++."
  LANGUAGES CXX)
enable_testing()
include(GNUInstallDirs)

set(CMAKE_COLOR_MAKEFILE ON)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake)

# Add third party code
add_subdirectory(dependencies)

# Find all the source files.
set(LIBRARY_NAME "lib${PROJECT_NAME}")
file(GLOB ${LIBRARY_NAME}_SOURCES "source/*.cc")

# define binary from the sources
add_library(${LIBRARY_NAME} STATIC ${${LIBRARY_NAME}_SOURCES})

# Turn on C++14
target_compile_features(${LIBRARY_NAME} PRIVATE cxx_std_14)

# turn on various warnings
if(MSVC)
  # USE_MATH_DEFINES is required for M_PI
  target_compile_options(${LIBRARY_NAME} PRIVATE /W4 /WX /D_USE_MATH_DEFINES
                                                 /bigobj /wd4244)
else()
  target_compile_options(${LIBRARY_NAME} PRIVATE -Wall -Wextra -pedantic
                                                 -Werror -Wno-sign-compare)
endif()

# Link
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
