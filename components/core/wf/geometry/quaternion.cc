// Copyright 2023 Gareth Cross
#include "wf/geometry/quaternion.h"

#include "wf/expressions/matrix.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"

namespace wf {

quaternion quaternion::from_name_prefix(const std::string_view name) {
  auto [w, x, y, z] = make_symbols(fmt::format("{}_w", name), fmt::format("{}_x", name),
                                   fmt::format("{}_y", name), fmt::format("{}_z", name));
  return {std::move(w), std::move(x), std::move(y), std::move(z)};
}

matrix_expr quaternion::to_vector_wxyz() const { return make_vector(w(), x(), y(), z()); }

matrix_expr quaternion::to_vector_xyzw() const { return make_vector(x(), y(), z(), w()); }

quaternion quaternion::subs(const scalar_expr& target, const scalar_expr& replacement) const {
  return quaternion(w().subs(target, replacement), x().subs(target, replacement),
                    y().subs(target, replacement), z().subs(target, replacement));
}

scalar_expr quaternion::squared_norm() const {
  return wxyz_[0] * wxyz_[0] + wxyz_[1] * wxyz_[1] + wxyz_[2] * wxyz_[2] + wxyz_[3] * wxyz_[3];
}

scalar_expr quaternion::norm() const { return sqrt(squared_norm()); }

matrix_expr quaternion::to_rotation_matrix() const {
  const scalar_expr x2 = x() * 2;
  const scalar_expr y2 = y() * 2;
  const scalar_expr z2 = z() * 2;
  const scalar_expr wx2 = x2 * w();
  const scalar_expr wy2 = y2 * w();
  const scalar_expr wz2 = z2 * w();
  const scalar_expr xx2 = x2 * x();
  const scalar_expr xy2 = y2 * x();
  const scalar_expr xz2 = z2 * x();
  const scalar_expr yy2 = y2 * y();
  const scalar_expr yz2 = z2 * y();
  const scalar_expr zz2 = z2 * z();
  // clang-format off
    return make_matrix(3, 3,
                        1 - yy2 - zz2,      xy2 - wz2,      xz2 + wy2,
                        xy2 + wz2,      1 - xx2 - zz2,      yz2 - wx2,
                        xz2 - wy2,          yz2 + wx2,  1 - xx2 - yy2
                        );
  // clang-format on
}

quaternion quaternion::from_angle_axis(const scalar_expr& angle, const scalar_expr& vx,
                                       const scalar_expr& vy, const scalar_expr& vz) {
  scalar_expr half_angle = angle / 2;
  scalar_expr sin_angle = sin(half_angle);
  return {cos(half_angle), vx * sin_angle, vy * sin_angle, vz * sin_angle};
}

quaternion quaternion::from_angle_axis(const scalar_expr& angle, const matrix_expr& v) {
  if (v.rows() != 3 || v.cols() != 1) {
    throw dimension_error("Axis vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
  }
  return from_angle_axis(angle, v[0], v[1], v[2]);
}

quaternion quaternion::from_rotation_vector(const scalar_expr& vx, const scalar_expr& vy,
                                            const scalar_expr& vz,
                                            const std::optional<scalar_expr>& epsilon) {
  const scalar_expr angle = sqrt(vx * vx + vy * vy + vz * vz);
  const scalar_expr half_angle = angle / 2;
  const scalar_expr sinc_half_angle = sin(half_angle) / angle;
  if (epsilon) {
    // When angle <= epsilon, we use the first order taylor series expansion of this method,
    // linearized about v = [0, 0, 0]. The series is projected back onto the unit norm quaternion.
    const quaternion q_small_angle = quaternion{1, vx / 2, vy / 2, vz / 2}.normalized();
    const boolean_expr condition = angle > *epsilon;
    return {
        where(condition, cos(half_angle), q_small_angle.w()),
        where(condition, vx * sinc_half_angle, q_small_angle.x()),
        where(condition, vy * sinc_half_angle, q_small_angle.y()),
        where(condition, vz * sinc_half_angle, q_small_angle.z()),
    };
  } else {
    return {
        cos(half_angle),
        vx * sinc_half_angle,
        vy * sinc_half_angle,
        vz * sinc_half_angle,
    };
  }
}

quaternion quaternion::from_rotation_vector(const matrix_expr& v,
                                            const std::optional<scalar_expr>& epsilon) {
  if (v.rows() != 3 || v.cols() != 1) {
    throw dimension_error("Rotation vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
  }
  return from_rotation_vector(v[0], v[1], v[2], epsilon);
}

// TODO: This should use the small angle approximation.
std::tuple<scalar_expr, matrix_expr> quaternion::to_angle_axis(
    std::optional<scalar_expr> epsilon) const {
  // We want to recover angle and axis from:
  // [cos(angle/2), vx * sin(angle/2), vy * sin(angle/2), vz * sin(angle/2)]
  // http://www.neil.dantam.name/note/dantam-quaternion.pdf (equation 19)
  const scalar_expr vector_norm = sqrt(x() * x() + y() * y() + z() * z());

  static const auto unit_x = make_vector(constants::one, constants::zero, constants::zero);
  if (is_zero(vector_norm)) {
    // If the norm is analytically zero, we can't do anything else here:
    return std::make_tuple(constants::zero, unit_x);
  }

  // Compute half-angle in range [0, pi/2] --> times 2 --> [0, pi]
  scalar_expr angle = atan2(vector_norm, abs(w())) * 2;
  const scalar_expr flipped_norm = where(w() < 0, -vector_norm, vector_norm);

  matrix_expr normalized_vector =
      make_vector(x() / flipped_norm, y() / flipped_norm, z() / flipped_norm);
  if (epsilon) {
    return std::make_tuple(where(vector_norm > *epsilon, angle, constants::zero),
                           where(vector_norm > *epsilon, normalized_vector, unit_x));
  } else {
    return std::make_tuple(std::move(angle), std::move(normalized_vector));
  }
}

matrix_expr quaternion::to_rotation_vector(std::optional<scalar_expr> epsilon) const {
  // The vector part norm is equal to sin(angle / 2)
  const scalar_expr vector_norm = sqrt(x() * x() + y() * y() + z() * z());
  const scalar_expr half_angle = atan2(vector_norm, w());
  const scalar_expr angle_over_norm = (half_angle * 2) / vector_norm;

  if (epsilon) {
    // When norm is < epsilon, we use the 1st order taylor series expansion of this function.
    // It is linearized about q = identity.
    const boolean_expr condition = vector_norm > *epsilon;
    return make_vector(where(condition, angle_over_norm * x(), 2 * x()),
                       where(condition, angle_over_norm * y(), 2 * y()),
                       where(condition, angle_over_norm * z(), 2 * z()));
  } else {
    return make_vector(angle_over_norm * x(), angle_over_norm * y(), angle_over_norm * z());
  }
}

quaternion quaternion::from_rotation_matrix(const matrix_expr& R_in) {
  if (R_in.rows() != 3 || R_in.cols() != 3) {
    throw dimension_error("Rotation matrix must be 3x3. Received: [{}, {}]", R_in.rows(),
                          R_in.cols());
  }
  const matrix& R = R_in.as_matrix();
  // clang-format off
  scalar_expr a = pow(R(0, 0) + R(1, 1) + R(2, 2) + 1, 2) +
           pow(R(2, 1) - R(1, 2), 2) +
           pow(R(0, 2) - R(2, 0), 2) +
           pow(R(1, 0) - R(0, 1), 2);
  scalar_expr b = pow(R(2, 1) - R(1, 2), 2) +
           pow(R(0, 0) - R(1, 1) - R(2, 2) + 1, 2) +
           pow(R(1, 0) + R(0, 1), 2) +
           pow(R(2, 0) + R(0, 2), 2);
  scalar_expr c = pow(R(0, 2) - R(2, 0), 2) +
           pow(R(1, 0) + R(0, 1), 2) +
           pow(R(1, 1) - R(0, 0) - R(2, 2) + 1, 2) +
           pow(R(2, 1) + R(1, 2), 2);
  scalar_expr d = pow(R(1, 0) - R(0, 1), 2) +
           pow(R(2, 0) + R(0, 2), 2) +
           pow(R(2, 1) + R(1, 2), 2) +
           pow(R(2, 2) - R(0, 0) - R(1, 1) + 1, 2);
  // clang-format on
  // We implement a signum without zeros:
  const scalar_expr sign_21 = 1 - 2 * iverson(R(2, 1) - R(1, 2) < 0);
  const scalar_expr sign_02 = 1 - 2 * iverson(R(0, 2) - R(2, 0) < 0);
  const scalar_expr sign_10 = 1 - 2 * iverson(R(1, 0) - R(0, 1) < 0);
  return {sqrt(a) / 4, sign_21 * sqrt(b) / 4, sign_02 * sqrt(c) / 4, sign_10 * sqrt(d) / 4};
}

matrix_expr quaternion::jacobian(const wf::matrix_expr& vars,
                                 const non_differentiable_behavior behavior) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw dimension_error("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                          vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.data(), behavior);
}

matrix_expr quaternion::right_retract_derivative() const {
  // Compute the expression `J` once, and substitute into it on subsequent calls:
  static const quaternion q_sub{
      make_unique_variable_symbol(number_set::real), make_unique_variable_symbol(number_set::real),
      make_unique_variable_symbol(number_set::real), make_unique_variable_symbol(number_set::real)};
  static const auto J = std::invoke([&]() -> matrix_expr {
    const auto vx = make_unique_variable_symbol(number_set::real);
    const auto vy = make_unique_variable_symbol(number_set::real);
    const auto vz = make_unique_variable_symbol(number_set::real);
    const quaternion q_perturb = q_sub * quaternion::from_rotation_vector(vx, vy, vz, 0);
    // Compute the Jacobian about 0:
    // clang-format off
    return substitute_variables(
        q_perturb.jacobian({vx, vy, vz}),
        {
            std::make_tuple(vx, constants::zero),
            std::make_tuple(vy, constants::zero),
            std::make_tuple(vz, constants::zero)
        });
    // clang-format on
  });
  // Substitute into J, replacing q_sub with the values in this quaternion:
  return substitute_variables(J, {
                                     std::make_tuple(q_sub.w(), w()),
                                     std::make_tuple(q_sub.x(), x()),
                                     std::make_tuple(q_sub.y(), y()),
                                     std::make_tuple(q_sub.z(), z()),
                                 });
}

matrix_expr quaternion::right_local_coordinates_derivative() const {
  static const quaternion q_sub{
      make_unique_variable_symbol(number_set::real), make_unique_variable_symbol(number_set::real),
      make_unique_variable_symbol(number_set::real), make_unique_variable_symbol(number_set::real)};
  static const auto J = std::invoke([&]() -> matrix_expr {
    const auto dw = make_unique_variable_symbol(number_set::real);
    const auto dx = make_unique_variable_symbol(number_set::real);
    const auto dy = make_unique_variable_symbol(number_set::real);
    const auto dz = make_unique_variable_symbol(number_set::real);
    const quaternion q_diff = q_sub.conjugate() * quaternion{q_sub.w() + dw, q_sub.x() + dx,
                                                             q_sub.y() + dy, q_sub.z() + dz};
    // clang-format off
    return substitute_variables(q_diff.to_rotation_vector(0).jacobian({dw, dx, dy, dz}),
                                {
                                    std::make_tuple(dw, 0),
                                    std::make_tuple(dx, 0),
                                    std::make_tuple(dy, 0),
                                    std::make_tuple(dz, 0)
                                });
    // clang-format on
  });
  return substitute_variables(J, {
                                     std::make_tuple(q_sub.w(), w()),
                                     std::make_tuple(q_sub.x(), x()),
                                     std::make_tuple(q_sub.y(), y()),
                                     std::make_tuple(q_sub.z(), z()),
                                 });
}

quaternion operator*(const quaternion& a, const quaternion& b) {
  return {a.w() * b.w() - a.x() * b.x() - a.y() * b.y() - a.z() * b.z(),
          a.w() * b.x() + a.x() * b.w() + a.y() * b.z() - a.z() * b.y(),
          a.w() * b.y() + a.y() * b.w() + a.z() * b.x() - a.x() * b.z(),
          a.w() * b.z() + a.z() * b.w() + a.x() * b.y() - a.y() * b.x()};
}

// A bit odd to put this here, since it technically doesn't have that much to do with
// quaternions. But it is a useful expression when dealing with rotations.
matrix_expr left_jacobian_of_so3(const matrix_expr& w, std::optional<scalar_expr> epsilon) {
  if (w.rows() != 3 || w.cols() != 1) {
    throw dimension_error("Rodrigues vector must be 3x1, received shape [{}, {}].", w.rows(),
                          w.cols());
  }
  const matrix& m = w.as_matrix();
  const scalar_expr& vx = m[0];
  const scalar_expr& vy = m[1];
  const scalar_expr& vz = m[2];
  const scalar_expr angle = sqrt(vx * vx + vy * vy + vz * vz);

  // Coefficients of the converged power series.
  // You can obtain these by integrating the exponential map of SO(3).
  const scalar_expr c0 = (1 - cos(angle)) / pow(angle, 2);
  const scalar_expr c1 = (angle - sin(angle)) / pow(angle, 3);
  static const scalar_expr c0_small_angle = constants::one / 2;  // lim[angle -> 0] c0
  static const scalar_expr c1_small_angle = constants::one / 6;  // lim[angle -> 0] c1

  // clang-format off
  const matrix_expr skew_v = make_matrix(3, 3,
                                          0, -vz,  vy,
                                         vz,   0, -vx,
                                        -vy,  vx,   0);
  // clang-format on
  static const auto I3 = make_identity(3);

  if (epsilon) {
    const boolean_expr condition = angle > *epsilon;
    return I3 + skew_v * where(condition, c0, c0_small_angle) +
           (skew_v * skew_v) * where(condition, c1, c1_small_angle);
  } else {
    return I3 + skew_v * c0 + (skew_v * skew_v) * c1;
  }
}

}  // namespace wf
