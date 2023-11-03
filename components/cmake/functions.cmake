# Function for defining code-generation tests. Each test consists of two stages:
# First the `xxx_generate` target generates a header named `generated.h`, and
# then the `xxx_evaluate` target includes the generated code and runs tests.
function(add_code_generation_test NAME GENERATION_SOURCE_FILE TEST_SOURCE_FILE)
  # First create a target that generates the code:
  set(generate_target ${NAME}_generate)
  set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

  add_executable(${generate_target} ${GENERATION_SOURCE_FILE})
  target_link_libraries(${generate_target} ${PROJECT_PREFIX}-test-support)
  target_include_directories(${generate_target} PRIVATE ${TEST_OUTPUT_DIR})

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

  # Then create a target that evaluates the result: TODO: Move custom_main.cc
  # somewhere more common?
  set(evaluate_target ${NAME}_evaluate)
  add_executable(
    ${evaluate_target}
    ${TEST_SOURCE_FILE}
    "${CMAKE_SOURCE_DIR}/components/math/tests/custom_main.cc"
    "${TEST_OUTPUT_DIR}/generated.h")
  target_link_libraries(${evaluate_target} ${PROJECT_PREFIX}-runtime
                        ${PROJECT_PREFIX}-test-support eigen gtest)
  target_include_directories(${evaluate_target} PRIVATE ${TEST_OUTPUT_DIR})

  # The evaluation target must depend on running the code-generation
  add_dependencies(${evaluate_target} ${generate_target}_run)

  # Create test case from the evaluation target:
  add_test(${evaluate_target} ${evaluate_target})
endfunction()
