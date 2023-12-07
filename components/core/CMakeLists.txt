# Add library:
add_library(
  ${LIBRARY_NAME} STATIC
  wf/absl_imports.h
  wf/assertions.h
  wf/code_generation/ast_conversion.h
  wf/code_generation/ast_conversion.cc
  wf/code_generation/ast.h
  wf/code_generation/ast.cc
  wf/code_generation/ast_formatters.h
  wf/code_generation/code_formatter.h
  wf/code_generation/cpp_code_generator.cc
  wf/code_generation/cpp_code_generator.h
  wf/code_generation/expr_from_ir.cc
  wf/code_generation/expression_group.h
  wf/code_generation/function_description.cc
  wf/code_generation/function_description.h
  wf/code_generation/ir_builder.cc
  wf/code_generation/ir_builder.h
  wf/code_generation/ir_types.h
  wf/code_generation/rust_code_generator.cc
  wf/code_generation/rust_code_generator.h
  wf/collect.cc
  wf/common_visitors.h
  wf/constants.cc
  wf/constants.h
  wf/derivative.cc
  wf/derivative.h
  wf/distribute.cc
  wf/enumerations.h
  wf/error_types.h
  wf/evaluate.cc
  wf/expression.cc
  wf/expression.h
  wf/expression_concept.h
  wf/expression_impl.h
  wf/expressions/addition.cc
  wf/expressions/addition.h
  wf/expressions/all_expressions.h
  wf/expressions/conditional.cc
  wf/expressions/conditional.h
  wf/expressions/derivative_expression.cc
  wf/expressions/derivative_expression.h
  wf/expressions/function_expressions.h
  wf/expressions/matrix.cc
  wf/expressions/matrix.h
  wf/expressions/multiplication.cc
  wf/expressions/multiplication.h
  wf/expressions/numeric_expressions.cc
  wf/expressions/numeric_expressions.h
  wf/expressions/power.cc
  wf/expressions/power.h
  wf/expressions/relational.cc
  wf/expressions/relational.h
  wf/expressions/special_constants.h
  wf/expressions/variable.cc
  wf/expressions/variable.h
  wf/fmt_imports.h
  wf/function_evaluator.h
  wf/function_evaluator_detail.h
  wf/functions.cc
  wf/functions.h
  wf/geometry/quaternion.cc
  wf/hashing.h
  wf/index_range.h
  wf/integer_utils.h
  wf/limits.cc
  wf/matrix_expression.cc
  wf/matrix_expression.h
  wf/matrix_functions.cc
  wf/matrix_functions.h
  wf/non_null_ptr.h
  wf/number_set.cc
  wf/operations.h
  wf/ordering.cc
  wf/output_annotations.h
  wf/plain_formatter.cc
  wf/plain_formatter.h
  wf/substitute.cc
  wf/template_utils.h
  wf/tree_formatter.cc
  wf/type_annotations.h
  wf/visitor_base.h
  wf/visitor_impl.h)

# Turn on C++17
target_compile_features(${LIBRARY_NAME} PUBLIC cxx_std_17)

# Set -fPIC since we will need to build shared libraries for python.
set_target_properties(${LIBRARY_NAME} PROPERTIES POSITION_INDEPENDENT_CODE ON)

if(MSVC)
  set(LIB_COMPILATION_FLAGS_PRIVATE /bigobj /EHs)
  set(LIB_COMPILATION_FLAGS_PUBLIC /Zc:__cplusplus)
else()
  set(LIB_COMPILATION_FLAGS_PRIVATE "")
  set(LIB_COMPILATION_FLAGS_PUBLIC "")
endif()

# Append our shared warning flags.
list(APPEND LIB_COMPILATION_FLAGS_PRIVATE ${SHARED_WARNING_FLAGS})
message(STATUS "Warning flags set to: ${SHARED_WARNING_FLAGS}")

target_compile_options(${LIBRARY_NAME} PRIVATE ${LIB_COMPILATION_FLAGS_PRIVATE})
target_compile_options(${LIBRARY_NAME} PUBLIC ${LIB_COMPILATION_FLAGS_PUBLIC})
target_link_libraries(${LIBRARY_NAME} ${PROJECT_PREFIX}-runtime
                      fmt::fmt-header-only absl::inlined_vector absl::span)
if(WIN32)
  target_compile_definitions(${LIBRARY_NAME} PUBLIC -D_USE_MATH_DEFINES)
endif()

target_include_directories(
  ${LIBRARY_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                         $<INSTALL_INTERFACE:include>)

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

# Add tests and benchmarks, if not building from scikit-build-core
if(NOT DEFINED SKBUILD_PROJECT_NAME)
  add_subdirectory(test_support)
  add_subdirectory(tests)
  add_subdirectory(benchmarks)
  add_subdirectory(examples)
endif()
