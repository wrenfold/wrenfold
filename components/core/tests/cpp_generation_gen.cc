// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"
#include "wf_test_support/code_gen_helpers.h"

#include "test_expressions.h"

class custom_cpp_code_generator final : public wf::cpp_code_generator {
 public:
  using cpp_code_generator::cpp_code_generator;
  using cpp_code_generator::operator();

  std::string operator()(const wf::custom_type& custom) const override {
    if (custom.is_native_type<wf::symbolic::Point2d>()) {
      return "wf::numeric::Point2d";
    }
    if (custom.is_native_type<wf::symbolic::Circle>()) {
      return "wf::numeric::Circle";
    }
    return cpp_code_generator::operator()(custom);
  }
};

int main() {
  using namespace wf;
  std::string code =
      "// Machine generated code.\n#pragma once\n#include <cmath>\n\n#include "
      "<wf_runtime/span.h>\n\n";

  code += "namespace gen {\n\n";

  custom_cpp_code_generator gen{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", "x", "y", "z");
  generate_func(
      gen, code,
      [](Expr theta, ta::static_matrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = vector_rotation_2d(theta, v);
        return std::make_tuple(v_rot.to_output_arg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
  generate_func(gen, code, &vector_norm_3d, "vector_norm_3d", "v");
  generate_func(gen, code, &heaviside, "heaviside", arg("x"));
  generate_func(gen, code, &exclusive_or, "exclusive_or", arg("x"), arg("y"));
  generate_func(gen, code, &signum_and_abs, "signum_and_abs", arg("x"));
  generate_func(gen, code, &atan2_with_derivatives, "atan2_with_derivatives", arg("y"), arg("x"));
  generate_func(gen, code, &nested_conditionals_1, "nested_conditionals_1", arg("x"), arg("y"));
  generate_func(gen, code, &nested_conditionals_2, "nested_conditionals_2", arg("x"), arg("y"));
  generate_func(gen, code, &create_rotation_matrix, "create_rotation_matrix", arg("w"));
  generate_func(gen, code, &rotation_vector_from_matrix, "rotation_vector_from_matrix", arg("R"));
  generate_func(gen, code, &no_required_outputs, "no_required_outputs", arg("x"));
  generate_func(gen, code, &custom_type_1, "custom_type_1", arg("p"));
  generate_func(gen, code, &custom_type_2, "custom_type_2", arg("theta"), arg("radius"));
  generate_func(gen, code, &nested_custom_type_1, "nested_custom_type_1", arg("c"), arg("p"));
  generate_func(gen, code, &custom_function_call_1, "custom_function_call_1", arg("x"), arg("y"));

  code += "} // namespace gen";

  std::ofstream output{GENERATOR_OUTPUT_FILE};
  output << code;
  output.flush();
  return 0;
}
