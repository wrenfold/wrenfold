# Add library:
add_library(
  wf_core STATIC
  wf/any_expression.h
  wf/boolean_expression.cc
  wf/boolean_expression.h
  wf/code_generation/argument.cc
  wf/code_generation/argument.h
  wf/code_generation/ast_conversion.cc
  wf/code_generation/ast_conversion.h
  wf/code_generation/ast_element.h
  wf/code_generation/ast_formatters.h
  wf/code_generation/ast.cc
  wf/code_generation/ast.h
  wf/code_generation/binarizer.cc
  wf/code_generation/binarizer.h
  wf/code_generation/control_flow_graph.cc
  wf/code_generation/control_flow_graph.h
  wf/code_generation/cpp_code_generator.cc
  wf/code_generation/cpp_code_generator.h
  wf/code_generation/declare_external_function.h
  wf/code_generation/expr_from_ir.cc
  wf/code_generation/expr_from_ir.h
  wf/code_generation/expression_group.h
  wf/code_generation/factorizer.cc
  wf/code_generation/factorizer.h
  wf/code_generation/function_description.cc
  wf/code_generation/function_description.h
  wf/code_generation/function_evaluator.h
  wf/code_generation/ir_block.cc
  wf/code_generation/ir_block.h
  wf/code_generation/ir_control_flow_converter.cc
  wf/code_generation/ir_control_flow_converter.h
  wf/code_generation/ir_form_visitor.cc
  wf/code_generation/ir_form_visitor.h
  wf/code_generation/ir_types.h
  wf/code_generation/ir_value.cc
  wf/code_generation/ir_value.h
  wf/code_generation/operation_counts.cc
  wf/code_generation/operation_counts.h
  wf/code_generation/python_code_generator.cc
  wf/code_generation/python_code_generator.h
  wf/code_generation/rust_code_generator.cc
  wf/code_generation/rust_code_generator.h
  wf/code_generation/type_registry.cc
  wf/code_generation/type_registry.h
  wf/code_generation/types.cc
  wf/code_generation/types.h
  wf/collect.cc
  wf/collect.h
  wf/compound_expression.cc
  wf/compound_expression.h
  wf/constants.cc
  wf/constants.h
  wf/cse.cc
  wf/cse.h
  wf/derivative.cc
  wf/derivative.h
  wf/distribute.cc
  wf/distribute.h
  wf/enumerations.h
  wf/evaluate.cc
  wf/expression_cache.h
  wf/expression_iteration.h
  wf/expression_variant.h
  wf/expression_visitor.h
  wf/expression.cc
  wf/expression.h
  wf/expressions/addition.cc
  wf/expressions/addition.h
  wf/expressions/all_expressions.h
  wf/expressions/compound_expression_element.cc
  wf/expressions/compound_expression_element.h
  wf/expressions/conditional.cc
  wf/expressions/conditional.h
  wf/expressions/custom_type_expressions.cc
  wf/expressions/custom_type_expressions.h
  wf/expressions/derivative_expression.cc
  wf/expressions/derivative_expression.h
  wf/expressions/external_function_invocation.cc
  wf/expressions/external_function_invocation.h
  wf/expressions/function_expressions.cc
  wf/expressions/function_expressions.h
  wf/expressions/matrix.cc
  wf/expressions/matrix.h
  wf/expressions/multiplication.cc
  wf/expressions/multiplication.h
  wf/expressions/stop_derivative.cc
  wf/expressions/stop_derivative.h
  wf/expressions/numeric_expressions.h
  wf/expressions/power.cc
  wf/expressions/power.h
  wf/expressions/relational.cc
  wf/expressions/relational.h
  wf/expressions/special_constants.h
  wf/expressions/substitute_expression.cc
  wf/expressions/substitute_expression.h
  wf/expressions/unevaluated.cc
  wf/expressions/unevaluated.h
  wf/expressions/variable.cc
  wf/expressions/variable.h
  wf/external_function.cc
  wf/external_function.h
  wf/functions.cc
  wf/functions.h
  wf/geometry/quaternion.cc
  wf/integer_utils.h
  wf/matrix_expression.cc
  wf/matrix_expression.h
  wf/matrix_functions.cc
  wf/matrix_functions.h
  wf/number_set.cc
  wf/numerical_casts.cc
  wf/numerical_casts.h
  wf/output_annotations.h
  wf/plain_formatter.cc
  wf/plain_formatter.h
  wf/substitute.cc
  wf/tree_formatter.cc
  wf/tree_formatter.h
  wf/type_annotations.h
  wf/utility_visitors.cc
  wf/utility_visitors.h
  wf/utility/algorithms.h
  wf/utility/assertions.h
  wf/utility/bitset_range.h
  wf/utility/checked_int.h
  wf/utility/checked_pointers.h
  wf/utility/error_types.h
  wf/utility/hashing.h
  wf/utility/index_range.h
  wf/utility/ordering.h
  wf/utility/overloaded_visit.h
  wf/utility/scoped_trace.cc
  wf/utility/scoped_trace.h
  wf/utility/stack_allocator.h
  wf/utility/static_vector.h
  wf/utility/strings.h
  wf/utility/third_party_imports.h
  wf/utility/traits.h
  wf/utility/type_list.h
  wf/utility/visit_switch.h
  wf/utility/zip_tuples.h)

# Turn on C++20
target_compile_features(wf_core PUBLIC cxx_std_20)

# Set -fPIC since we will need to build shared libraries for python.
set_target_properties(wf_core PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Append our shared warning flags.
message(STATUS "Warning flags set to: ${WF_SHARED_WARNING_FLAGS}")
target_compile_options(wf_core PRIVATE ${WF_SHARED_WARNING_FLAGS})

if(MSVC)
  target_compile_options(wf_core PRIVATE /bigobj /EHs)
endif()

target_link_libraries(wf_core wf_coverage_config)
target_link_libraries(wf_core wf_runtime fmt::fmt-header-only
                      absl::inlined_vector absl::span absl::flat_hash_set)
if(WIN32)
  target_compile_definitions(wf_core PUBLIC -D_USE_MATH_DEFINES)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "Debug build detected, enabling WF_DEBUG")
  target_compile_definitions(wf_core PUBLIC -DWF_DEBUG)
  if(DEFINED SKBUILD_PROJECT_NAME)
    # It would appear on windows that scikit-build-core builds in Debug mode,
    # irrespective of cmake.build-type. We definitely want to catch this. If you
    # actually want to produce Debug build wheels, comment this out.
    message(
      FATAL_ERROR
        "In Debug build, but running from scikit-build-core. This is probably a mistake."
    )
  endif()
endif()

option(WF_BUILD_WITH_TRACING "Enable traces for profiling." OFF)
if(WF_BUILD_WITH_TRACING)
  message(STATUS "Building with tracing enabled.")
  target_compile_definitions(wf_core PUBLIC -DWF_ENABLE_TRACING)
endif()

target_include_directories(
  wf_core PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:include>)

if(WF_BUILD_EXTRAS)
  add_subdirectory(test_support)
  add_subdirectory(tests)
  add_subdirectory(benchmarks)
endif()
