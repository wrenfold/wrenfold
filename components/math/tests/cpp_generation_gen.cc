// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "code_gen_helpers.h"
#include "code_generation/cpp_code_generator.h"
#include "test_expressions.h"

int main() {
  using namespace math;
  std::string code = "// Machine generated code.\n#include <cmath>\n\n#include <span.h>\n\n";

  code += "namespace gen {\n\n";

  CppCodeGenerator gen{};
  generate_func(gen, code, &simple_multiply_add, "simple_multiply_add", "x", "y", "z");
  generate_func(
      gen, code,
      [](Expr theta, ta::StaticMatrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = vector_rotation_2d(theta, v);
        return std::make_tuple(v_rot.to_output_arg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
  generate_func(gen, code, &vector_norm_3d, "vector_norm_3d", "v");
  generate_func(gen, code, &heaviside, "heaviside", Arg("x"));
  generate_func(gen, code, &exclusive_or, "exclusive_or", Arg("x"), Arg("y"));
  generate_func(gen, code, &handwritten_signum, "handwritten_signum", Arg("x"));
  generate_func(gen, code, &handwritten_abs, "handwritten_abs", Arg("x"));
  generate_func(gen, code, &atan2_with_derivatives, "atan2_with_derivatives", Arg("y"), Arg("x"));

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}