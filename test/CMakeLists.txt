# Create a library with reusable test utilities.
set(TEST_LIBRARY_NAME ${PROJECT_NAME}_test_utils)
add_library(${TEST_LIBRARY_NAME} STATIC test_helpers.cc)
target_link_libraries(${TEST_LIBRARY_NAME} ${LIBRARY_NAME} gtest_main eigen)
target_compile_options(${TEST_LIBRARY_NAME} PUBLIC ${MATH_COMPILATION_FLAGS})

get_target_property(EIGEN_INCLUDE_DIRS eigen INTERFACE_INCLUDE_DIRECTORIES)
set(TEST_CONFIG_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/test_config)
configure_file(config/config_variables.h.in
               ${TEST_CONFIG_OUTPUT_DIR}/config_variables.h)

# Specify individual tests here:
set(${PROJECT_NAME}_TEST_SOURCES
    plain_formatter_test.cc
    scalar_operations_test.cc
    derivatives_test.cc
    integer_utils_test.cc
    numeric_expressions_test.cc
    functions_test.cc
    matrix_operations_test.cc
    code_generation_test.cc
    substitute_test.cc
    owning_ptr_test.cc)

foreach(test_file ${${PROJECT_NAME}_TEST_SOURCES})
  # get just the filename w/o extension
  get_filename_component(test_name ${test_file} NAME_WE)

  # add test executable for every file
  message(STATUS "Discovered test: ${test_name}")
  add_executable(${test_name} ${test_file} custom_main.cc)
  target_link_libraries(${test_name} ${TEST_LIBRARY_NAME} gtest)
  target_include_directories(${test_name} PRIVATE ${TEST_CONFIG_OUTPUT_DIR})

  add_test(${test_name} ${test_name})
endforeach(test_file)

# Function for defining code-generation tests, which are built and run twice.
# The first pass performs code-generation, while the second compiles w/ the
# output.
function(add_code_generation_test NAME GENERATION_SOURCE_FILE TEST_SOURCE_FILE)
  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")
  configure_file(config/config_variables.h.in
                 ${TEST_OUTPUT_DIR}/config_variables.h)

  add_executable(${generate_target} ${GENERATION_SOURCE_FILE})
  target_link_libraries(${generate_target} ${TEST_LIBRARY_NAME})
  target_include_directories(${generate_target} PRIVATE ${TEST_OUTPUT_DIR})
  target_compile_definitions(${generate_target} PRIVATE GENERATION_TIME)

  # Add a target that runs the generation:
  add_custom_command(
    OUTPUT "${TEST_OUTPUT_DIR}/generated.h"
    COMMAND
      "$<TARGET_FILE_DIR:${generate_target}>/$<TARGET_FILE_NAME:${generate_target}>"
    WORKING_DIRECTORY "${TEST_OUTPUT_DIR}"
    COMMENT "Run code-generation for test ${NAME}"
    DEPENDS ${generate_target})
  add_custom_target(${generate_target}_run
                    DEPENDS "${TEST_OUTPUT_DIR}/generated.h")

  # Then create a target that evaluates the result:
  set(evaluate_target ${NAME}_evaluate)
  add_executable(${evaluate_target} ${TEST_SOURCE_FILE} custom_main.cc)
  target_link_libraries(${evaluate_target} ${TEST_LIBRARY_NAME} gtest eigen)
  target_include_directories(${evaluate_target} PRIVATE ${TEST_OUTPUT_DIR})
  target_compile_definitions(${evaluate_target} PRIVATE EVALUATION_TIME)

  # The evaluation target must depend on running the code-generation
  add_dependencies(${evaluate_target} ${generate_target}_run)

  # Create test case from the evaluation target:
  add_test(${evaluate_target} ${evaluate_target})
endfunction()

add_code_generation_test(generate_cpp_test cpp_generation_gen.cc
                         cpp_generation_test.cc)
