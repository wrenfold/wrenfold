// Copyright 2024 Gareth Cross
#pragma once

#include "test_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

namespace wf {

// Generate all the expressions in `test_expressions.h`
template <typename Generator>
std::string generate_test_expressions(Generator gen) {
  std::string code{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", arg("x"), arg("y"),
                arg("z"));
  generate_func(gen, code, &vector_rotation_2d, "vector_rotation_2d", arg("theta"), arg("v"));
  generate_func(gen, code, &vector_norm_3d, "vector_norm_3d", arg("v"));
  generate_func(gen, code, &heaviside, "heaviside", arg("x"));
  generate_func(gen, code, &exclusive_or, "exclusive_or", arg("x"), arg("y"));
  generate_func(gen, code, &signum_and_abs, "signum_and_abs", arg("x"));
  generate_func(gen, code, &floor_test, "floor_test", arg("x"));
  generate_func(gen, code, &atan2_with_derivatives, "atan2_with_derivatives", arg("y"), arg("x"));
  generate_func(gen, code, &nested_conditionals_1, "nested_conditionals_1", arg("x"), arg("y"));
  generate_func(gen, code, &nested_conditionals_2, "nested_conditionals_2", arg("x"), arg("y"));
  generate_func(gen, code, &create_rotation_matrix, "create_rotation_matrix", arg("w"));
  generate_func(gen, code, &rotation_vector_from_matrix, "rotation_vector_from_matrix", arg("R"));
  generate_func(gen, code, &no_required_outputs, "no_required_outputs", arg("x"));
  generate_func(gen, code, &custom_type_1, "custom_type_1", arg("p"));
  generate_func(gen, code, &custom_type_2, "custom_type_2", arg("theta"), arg("radius"));
  generate_func(gen, code, &nested_custom_type_1, "nested_custom_type_1", arg("c"), arg("p"));
  generate_func(gen, code, &external_function_call_1, "external_function_call_1", arg("x"),
                arg("y"));
  generate_func(gen, code, &external_function_call_2, "external_function_call_2", arg("u"),
                arg("v"));
  generate_func(gen, code, &external_function_call_3, "external_function_call_3", arg("x"),
                arg("v"));
  generate_func(gen, code, &external_function_call_4, "external_function_call_4", arg("a"),
                arg("b"));
  generate_func(gen, code, &external_function_call_5, "external_function_call_5", arg("c"),
                arg("x"), arg("y"));
  generate_func(gen, code, &external_function_call_6, "external_function_call_6", arg("x"),
                arg("y"));
  return code;
}

}  // namespace wf