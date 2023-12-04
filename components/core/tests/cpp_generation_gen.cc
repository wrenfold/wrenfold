// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"

#include "test_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

int main() {
  using namespace math;
  std::string code =
      "// Machine generated code.\n#include <cmath>\n\n#include <wf_runtime/span.h>\n\n";

  code += "namespace gen {\n\n";

  cpp_code_generator gen{};
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

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
