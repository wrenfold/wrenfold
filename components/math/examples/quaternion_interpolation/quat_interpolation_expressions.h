// Copyright 2023 Gareth Cross
#include "geometry/quaternion.h"
#include "output_annotations.h"
#include "type_annotations.h"

namespace ta = math::type_annotations;

namespace math {

// Interpolate between two quaternions, `q0` and `q1` (passed as scalar-last vectors).
// Outputs the interpolated quaternion, as well as the tangent space derivatives.
auto quaternion_interpolation(ta::StaticMatrix<4, 1> q0_vec, ta::StaticMatrix<4, 1> q1_vec,
                              Expr alpha) {
  using namespace matrix_operator_overloads;

  const Quaternion q0 = Quaternion::from_vector_xyzw(q0_vec);
  const Quaternion q1 = Quaternion::from_vector_xyzw(q1_vec);
  const MatrixExpr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const Quaternion q_interp =
      q0 * Quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  ta::StaticMatrix<3, 3> D_q0 = q_interp.right_local_coordinates_derivative() *
                                q_interp.to_vector_wxyz().jacobian(q0.wxyz()) *
                                q0.right_retract_derivative();
  ta::StaticMatrix<3, 3> D_q1 = q_interp.right_local_coordinates_derivative() *
                                q_interp.to_vector_wxyz().jacobian(q1.wxyz()) *
                                q1.right_retract_derivative();

  return std::make_tuple(OutputArg("q_out", ta::StaticMatrix<4, 1>(q_interp.to_vector_xyzw())),
                         OptionalOutputArg("D_q0", std::move(D_q0)),
                         OptionalOutputArg("D_q1", std::move(D_q1)));
}

}  // namespace math