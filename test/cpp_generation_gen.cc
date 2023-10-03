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
  GenerateFunc(gen, code, &SimpleMultiplyAdd, "simple_multiply_add", "x", "y", "z");
  GenerateFunc(
      gen, code,
      [](Expr theta, ta::StaticMatrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = VectorRotation2D(theta, v);
        return std::make_tuple(v_rot.to_output_arg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
  GenerateFunc(gen, code, &VectorNorm3D, "vector_norm_3d", "v");
  GenerateFunc(gen, code, &Heaviside, "heaviside", Arg("x"));
  GenerateFunc(gen, code, &ExclusiveOr, "exclusive_or", Arg("x"), Arg("y"));
  GenerateFunc(gen, code, &HandwrittenSignum, "handwritten_signum", Arg("x"));
  GenerateFunc(gen, code, &HandwrittenAbs, "handwritten_abs", Arg("x"));
  GenerateFunc(gen, code, &Atan2WithDerivatives, "atan2_with_derivatives", Arg("y"), Arg("x"));

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
