// Copyright 2023 Gareth Cross
#include <filesystem>
#include <fstream>

#include "code_gen_helpers.h"
#include "code_generation/cpp_code_generator.h"
#include "geometry/quaternion.h"
#include "type_annotations.h"

using namespace math;
namespace ta = math::type_annotations;

auto quaternion_interpolation(ta::StaticMatrix<4, 1> q0_vec, ta::StaticMatrix<4, 1> q1_vec,
                              Expr alpha) {
  using namespace matrix_operator_overloads;

  const Quaternion q0 = Quaternion::from_vector_wxyz(q0_vec);
  const Quaternion q1 = Quaternion::from_vector_wxyz(q1_vec);
  const MatrixExpr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const Quaternion q_interp =
      q0 * Quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  ta::StaticMatrix<4, 1> q_out = q_interp.to_vector_wxyz();

  // TODO: Add a simpler interface for getting the tangent space derivative.
  ta::StaticMatrix<4, 3> D_q0 = q_out.inner().jacobian(q0_vec) * q0.right_tangent_derivative();
  ta::StaticMatrix<4, 3> D_q1 = q_out.inner().jacobian(q1_vec) * q1.right_tangent_derivative();

  return std::make_tuple(OutputArg("q_out", std::move(q_out)),
                         OptionalOutputArg("D_q0", std::move(D_q0)),
                         OptionalOutputArg("D_q1", std::move(D_q1)));
}

int main() {
  using namespace math;
  std::string code = "// Machine generated code.\n#include <cmath>\n\n#include <span.h>\n\n";

  code += "namespace gen {\n\n";

  CppCodeGenerator gen{};
  generate_func(gen, code, &quaternion_interpolation, "quaternion_interpolation", "q0", "q1",
                "alpha");

  code += "} // namespace gen";

  std::ofstream output{"generated.h"};
  output << code;
  output.flush();
  return 0;
}
