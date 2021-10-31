include(FetchContent)

# Download google test.
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG release-1.10.0
)
FetchContent_GetProperties(googletest)
if (NOT googletest_POPULATED)
    FetchContent_Populate(googletest)
    # Disable gmock and install of gtest
    set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)
    set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
    add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR} EXCLUDE_FROM_ALL)
endif ()

set(TEST_LIBRARY_NAME ${PROJECT_NAME}_TEST_UTIL)
set(${TEST_LIBRARY_NAME}_SOURCES "test_helpers.cc")
add_library(${TEST_LIBRARY_NAME} STATIC ${${TEST_LIBRARY_NAME}_SOURCES})
target_link_libraries(${TEST_LIBRARY_NAME} ${LIBRARY_NAME} gtest_main fmt)

if (MSVC)
    # USE_MATH_DEFINES is required for M_PI
    target_compile_options(${TEST_LIBRARY_NAME} PUBLIC /W4 /WX /D_USE_MATH_DEFINES /bigobj /wd4244)
else ()
    target_compile_options(${TEST_LIBRARY_NAME} PUBLIC
            -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
endif ()

if (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_definitions(${TEST_LIBRARY_NAME} PUBLIC NDEBUG)
endif ()

# Find all the test files.
FILE(GLOB ${PROJECT_NAME}_TEST_SOURCES "*_test.cc")
foreach (test_file ${${PROJECT_NAME}_TEST_SOURCES})
    # get just the filename w/o extension
    get_filename_component(test_name ${test_file} NAME_WE)

    # add test executable for every file
    message(STATUS "Discovered test: ${test_name}")
    add_executable(${test_name} ${test_file})
    target_link_libraries(${test_name} ${TEST_LIBRARY_NAME} gtest_main fmt)

    # turn on maximum warnings
#    if (MSVC)
#        # USE_MATH_DEFINES is required for M_PI
#        target_compile_options(${test_name} PRIVATE /W4 /WX /D_USE_MATH_DEFINES /bigobj /wd4244)
#    else ()
#        target_compile_options(${test_name} PRIVATE
#                -Wall -Wextra -pedantic -Werror -Wno-sign-compare)
#    endif ()

    if (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_definitions(${test_name} PUBLIC NDEBUG)
    endif ()

    add_test(${test_name} ${test_name})
endforeach (test_file)
