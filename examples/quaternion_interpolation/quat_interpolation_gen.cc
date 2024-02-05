// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"

#include "quat_interpolation_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

int main() {
  using namespace wf;
  std::string code =
      "// Machine generated code.\n#include <cmath>\n\n#include <wf_runtime/span.h>\n\n";

  code += "namespace gen {\n\n";

  cpp_code_generator gen{};
  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec, scalar_expr alpha) {
        return quaternion_interpolation(q0_vec, q1_vec, alpha, 1.0e-16);
      },
      "quaternion_interpolation", "q0", "q1", "alpha");
  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec, scalar_expr alpha) {
        return quaternion_interpolation(q0_vec, q1_vec, alpha, std::nullopt);
      },
      "quaternion_interpolation_no_conditional", "q0", "q1", "alpha");

  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec, scalar_expr alpha) {
        return std::get<0>(quaternion_interpolation(q0_vec, q1_vec, alpha, 1.0e-16));
      },
      "quaternion_interpolation_no_diff", "q0", "q1", "alpha");
  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec, scalar_expr alpha) {
        return std::get<0>(quaternion_interpolation(q0_vec, q1_vec, alpha, std::nullopt));
      },
      "quaternion_interpolation_no_conditional_no_diff", "q0", "q1", "alpha");

  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec) {
        return quaternion_local_coordinates(q0_vec, q1_vec, 1.0e-16);
      },
      "quaternion_local_coordinates", "q0", "q1");

  generate_func(
      gen, code,
      [](ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec) {
        return quaternion_local_coordinates(q0_vec, q1_vec, std::nullopt);
      },
      "quaternion_local_coordinates_no_conditional", "q0", "q1");

  code += "} // namespace gen";

  std::ofstream output{GENERATOR_OUTPUT_FILE};
  output << code;
  output.flush();
  return 0;
}
