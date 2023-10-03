# Add library:
add_library(
  ${LIBRARY_NAME} STATIC
  source/code_generation/ast.cc
  source/code_generation/ir_builder.cc
  source/code_generation/expr_from_ir.cc
  source/code_generation/cpp_code_generator.cc
  source/code_generation/rust_code_generator.cc
  source/collect.cc
  source/constants.cc
  source/derivative.cc
  source/distribute.cc
  source/evaluate.cc
  source/expression.cc
  source/expressions/addition.cc
  source/expressions/conditional.cc
  source/expressions/derivative_expression.cc
  source/expressions/matrix.cc
  source/expressions/multiplication.cc
  source/expressions/numeric_expressions.cc
  source/expressions/power.cc
  source/expressions/relational.cc
  source/functions.cc
  source/matrix_expression.cc
  source/matrix_functions.cc
  source/ordering.cc
  source/plain_formatter.cc
  source/substitute.cc
  source/tree_formatter.cc)

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

# TODO: Properly nest things in a named directory.
target_include_directories(
  ${LIBRARY_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                         $<INSTALL_INTERFACE:include>)

# Specify public headers
set(${LIBRARY_NAME}_HEADERS
    include/absl_imports.h
    include/assertions.h
    include/code_generation/code_formatter.h
    include/code_generation/ast.h
    include/code_generation/ast_formatters.h
    include/code_generation/cpp_code_generator.h
    include/code_generation/expression_group.h
    include/code_generation/ir_builder.h
    include/code_generation/ir_types.h
    include/code_generation/rust_code_generator.h
    include/common_visitors.h
    include/constants.h
    include/enumerations.h
    include/error_types.h
    include/expression.h
    include/expressions/addition.h
    include/expressions/all_expressions.h
    include/expressions/conditional.h
    include/expressions/derivative_expression.h
    include/expressions/function_argument.h
    include/expressions/function_expressions.h
    include/expressions/matrix.h
    include/expressions/multiplication.h
    include/expressions/numeric_expressions.h
    include/expressions/power.h
    include/expressions/relational.h
    include/expressions/special_constants.h
    include/expressions/variable.h
    include/expression_concept.h
    include/expression_impl.h
    include/functions.h
    include/output_annotations.h
    include/fmt_imports.h
    include/function_evaluator.h
    include/function_evaluator_detail.h
    include/hashing.h
    include/index_range.h
    include/integer_utils.h
    include/matrix_expression.h
    include/matrix_functions.h
    include/non_null_ptr.h
    include/operations.h
    include/plain_formatter.h
    include/template_utils.h
    include/type_annotations.h
    include/visitor_base.h
    include/visitor_impl.h)

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
