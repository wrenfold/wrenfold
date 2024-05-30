// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <filesystem>
#include <fstream>

#include "wf/code_generation/cpp_code_generator.h"

#include "quat_interpolation_expressions.h"
#include "wf_test_support/code_gen_helpers.h"

// This example generates using C++. This is not the expected use-case - most users are expected to
// interact with the framework through Python, but it is possible to do.
int main() {
  using namespace wf;
  std::string code{};
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
  std::ofstream output{GENERATOR_OUTPUT_FILE};
  output << cpp_code_generator::apply_preamble(code, "gen");
  output.flush();
  return 0;
}
