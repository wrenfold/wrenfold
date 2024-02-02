// Copyright 2023 Gareth Cross
#pragma once
#include "wf/geometry/quaternion.h"
#include "wf/output_annotations.h"
#include "wf/type_annotations.h"

namespace ta = wf::type_annotations;

namespace wf {

// Interpolate between two quaternions, `q0` and `q1` (passed as scalar-last vectors).
// Outputs the interpolated quaternion, as well as the tangent space derivatives.
auto quaternion_interpolation(ta::static_matrix<4, 1> q0_vec, ta::static_matrix<4, 1> q1_vec,
                              Expr alpha) {
  using namespace matrix_operator_overloads;

  const quaternion q0 = quaternion::from_vector_xyzw(q0_vec);
  const quaternion q1 = quaternion::from_vector_xyzw(q1_vec);
  const matrix_expr q_delta_tangent = (q0.conjugate() * q1).to_rotation_vector(1.0e-16);
  const quaternion q_interp =
      q0 * quaternion::from_rotation_vector(q_delta_tangent * alpha, 1.0e-16);

  ta::static_matrix<3, 3> D_q0 = q_interp.right_local_coordinates_derivative() *
                                 q_interp.jacobian(q0) * q0.right_retract_derivative();
  ta::static_matrix<3, 3> D_q1 = q_interp.right_local_coordinates_derivative() *
                                 q_interp.jacobian(q1) * q1.right_retract_derivative();

  return std::make_tuple(output_arg("q_out", ta::static_matrix<4, 1>(q_interp.to_vector_xyzw())),
                         optional_output_arg("D_q0", std::move(D_q0)),
                         optional_output_arg("D_q1", std::move(D_q1)));
}

}  // namespace wf
