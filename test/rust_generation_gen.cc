// Copyright 2023 Gareth Cross
#include <fstream>

#include "code_gen_helpers.h"
#include "code_generation/rust_code_generator.h"
#include "test_expressions.h"

int main() {
  using namespace math;
  std::string code = "// Machine generated code.\n\n";

  RustCodeGenerator gen{};
  GenerateFunc(gen, code, &SimpleMultiplyAdd, "simple_multiply_add", "x", "y", "z");
  GenerateFunc(
      gen, code,
      [](Expr theta, ta::StaticMatrix<2, 1> v) {
        auto [v_rot, v_rot_D_theta] = VectorRotation2D(theta, v);
        return std::make_tuple(v_rot.ToOutputArg("v_rot"), std::move(v_rot_D_theta));
      },
      "vector_rotation_2d", "theta", "v");
  GenerateFunc(gen, code, &VectorNorm3D, "vector_norm_3d", "v");
  GenerateFunc(gen, code, &Heaviside, "heaviside", Arg("x"));
  GenerateFunc(gen, code, &ExclusiveOr, "exclusive_or", Arg("x"), Arg("y"));
  GenerateFunc(gen, code, &HandwrittenSignum, "handwritten_signum", Arg("x"));
  GenerateFunc(gen, code, &HandwrittenAbs, "handwritten_abs", Arg("x"));
  GenerateFunc(gen, code, &Atan2WithDerivatives, "atan2_with_derivatives", Arg("y"), Arg("x"));

  std::ofstream output{"generated.rs"};
  output << code;
  output.flush();
  return 0;
}