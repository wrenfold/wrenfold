// Copyright 2023 Gareth Cross
#include "geometry/quaternion.h"

#include "expressions/matrix.h"
#include "functions.h"
#include "matrix_functions.h"

namespace math {

Quaternion Quaternion::from_name_prefix(const std::string_view name) {
  auto [w, x, y, z] = make_symbols(fmt::format("{}_w", name), fmt::format("{}_x", name),
                                   fmt::format("{}_y", name), fmt::format("{}_z", name));
  return {std::move(w), std::move(x), std::move(y), std::move(z)};
}

MatrixExpr Quaternion::to_vector_wxyz() const { return make_vector(w(), x(), y(), z()); }

MatrixExpr Quaternion::to_vector_xyzw() const { return make_vector(x(), y(), z(), w()); }

Quaternion Quaternion::subs(const Expr& target, const Expr& replacement) const {
  return Quaternion(w().subs(target, replacement), x().subs(target, replacement),
                    y().subs(target, replacement), z().subs(target, replacement));
}

Expr Quaternion::squared_norm() const {
  return wxyz_[0] * wxyz_[0] + wxyz_[1] * wxyz_[1] + wxyz_[2] * wxyz_[2] + wxyz_[3] * wxyz_[3];
}

Expr Quaternion::norm() const { return sqrt(squared_norm()); }

MatrixExpr Quaternion::to_rotation_matrix() const {
  const Expr x2 = x() * 2;
  const Expr y2 = y() * 2;
  const Expr z2 = z() * 2;
  const Expr wx2 = x2 * w();
  const Expr wy2 = y2 * w();
  const Expr wz2 = z2 * w();
  const Expr xx2 = x2 * x();
  const Expr xy2 = y2 * x();
  const Expr xz2 = z2 * x();
  const Expr yy2 = y2 * y();
  const Expr yz2 = z2 * y();
  const Expr zz2 = z2 * z();
  // clang-format off
    return make_matrix(3, 3,
                        1 - yy2 - zz2,      xy2 - wz2,      xz2 + wy2,
                        xy2 + wz2,      1 - xx2 - zz2,      yz2 - wx2,
                        xz2 - wy2,          yz2 + wx2,  1 - xx2 - yy2
                        );
  // clang-format on
}

Quaternion Quaternion::from_angle_axis(const Expr& angle, const Expr& vx, const Expr& vy,
                                       const Expr& vz) {
  Expr half_angle = angle / 2;
  Expr sin_angle = sin(half_angle);
  return {cos(half_angle), vx * sin_angle, vy * sin_angle, vz * sin_angle};
}

Quaternion Quaternion::from_angle_axis(const Expr& angle, const MatrixExpr& v) {
  if (v.rows() != 3 || v.cols() != 1) {
    throw DimensionError("Axis vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
  }
  return from_angle_axis(angle, v[0], v[1], v[2]);
}

Quaternion Quaternion::from_rotation_vector(const Expr& vx, const Expr& vy, const Expr& vz,
                                            const std::optional<Expr> epsilon) {
  const Expr angle = sqrt(vx * vx + vy * vy + vz * vz);
  const Expr half_angle = angle / 2;
  const Expr sinc_half_angle = sin(half_angle) / angle;
  if (epsilon) {
    // When angle <= epsilon, we use the first order taylor series expansion of this method,
    // linearized about v = [0, 0, 0]. The series is projected back onto the unit norm quaternion.
    const Quaternion q_small_angle = Quaternion{1, vx / 2, vy / 2, vz / 2}.normalized();
    return {
        where(angle > *epsilon, cos(half_angle), q_small_angle.w()),
        where(angle > *epsilon, vx * sinc_half_angle, q_small_angle.x()),
        where(angle > *epsilon, vy * sinc_half_angle, q_small_angle.y()),
        where(angle > *epsilon, vz * sinc_half_angle, q_small_angle.z()),
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

Quaternion Quaternion::from_rotation_vector(const MatrixExpr& v, std::optional<Expr> epsilon) {
  if (v.rows() != 3 || v.cols() != 1) {
    throw DimensionError("Rotation vector must be 3x1. Received: [{}, {}]", v.rows(), v.cols());
  }
  return from_rotation_vector(v[0], v[1], v[2], std::move(epsilon));
}

// TODO: This should use the small angle approximation.
std::tuple<Expr, MatrixExpr> Quaternion::to_angle_axis(std::optional<Expr> epsilon) const {
  // We want to recover angle and axis from:
  // [cos(angle/2), vx * sin(angle/2), vy * sin(angle/2), vz * sin(angle/2)]
  // http://www.neil.dantam.name/note/dantam-quaternion.pdf (equation 19)
  const Expr vector_norm = sqrt(x() * x() + y() * y() + z() * z());

  static const auto unit_x = make_vector(Constants::One, Constants::Zero, Constants::Zero);
  if (is_zero(vector_norm)) {
    // If the norm is analytically zero, we can't do anything else here:
    return std::make_tuple(Constants::Zero, unit_x);
  }

  // Compute half-angle in range [0, pi/2] --> times 2 --> [0, pi]
  Expr angle = atan2(vector_norm, abs(w())) * 2;
  const Expr flipped_norm = where(w() < 0, -vector_norm, vector_norm);

  MatrixExpr normalized_vector =
      make_vector(x() / flipped_norm, y() / flipped_norm, z() / flipped_norm);
  if (epsilon) {
    return std::make_tuple(where(vector_norm > *epsilon, angle, Constants::Zero),
                           where(vector_norm > *epsilon, normalized_vector, unit_x));
  } else {
    return std::make_tuple(std::move(angle), std::move(normalized_vector));
  }
}

MatrixExpr Quaternion::to_rotation_vector(std::optional<Expr> epsilon) const {
  // The vector part norm is equal to sin(angle / 2)
  const Expr vector_norm = sqrt(x() * x() + y() * y() + z() * z());
  const Expr half_angle = atan2(vector_norm, w());
  const Expr angle_over_norm = (half_angle * 2) / vector_norm;

  if (epsilon) {
    // When norm is < epsilon, we use the 1st order taylor series expansion of this function.
    // It is linearized about q = identity.
    return make_vector(where(vector_norm > *epsilon, angle_over_norm * x(), 2 * x()),
                       where(vector_norm > *epsilon, angle_over_norm * y(), 2 * y()),
                       where(vector_norm > *epsilon, angle_over_norm * z(), 2 * z()));
  } else {
    return make_vector(angle_over_norm * x(), angle_over_norm * y(), angle_over_norm * z());
  }
}

Quaternion Quaternion::from_rotation_matrix(const MatrixExpr& R_in) {
  if (R_in.rows() != 3 || R_in.cols() != 3) {
    throw DimensionError("Rotation matrix must be 3x3. Received: [{}, {}]", R_in.rows(),
                         R_in.cols());
  }
  const Matrix& R = R_in.as_matrix();
  // clang-format off
  Expr a = pow(R(0, 0) + R(1, 1) + R(2, 2) + 1, 2) +
           pow(R(2, 1) - R(1, 2), 2) +
           pow(R(0, 2) - R(2, 0), 2) +
           pow(R(1, 0) - R(0, 1), 2);
  Expr b = pow(R(2, 1) - R(1, 2), 2) +
           pow(R(0, 0) - R(1, 1) - R(2, 2) + 1, 2) +
           pow(R(1, 0) + R(0, 1), 2) +
           pow(R(2, 0) + R(0, 2), 2);
  Expr c = pow(R(0, 2) - R(2, 0), 2) +
           pow(R(1, 0) + R(0, 1), 2) +
           pow(R(1, 1) - R(0, 0) - R(2, 2) + 1, 2) +
           pow(R(2, 1) + R(1, 2), 2);
  Expr d = pow(R(1, 0) - R(0, 1), 2) +
           pow(R(2, 0) + R(0, 2), 2) +
           pow(R(2, 1) + R(1, 2), 2) +
           pow(R(2, 2) - R(0, 0) - R(1, 1) + 1, 2);
  // clang-format on
  // We implement a signum without zeros:
  const Expr sign_21 = 1 - 2 * cast_int_from_bool(R(2, 1) - R(1, 2) < 0);
  const Expr sign_02 = 1 - 2 * cast_int_from_bool(R(0, 2) - R(2, 0) < 0);
  const Expr sign_10 = 1 - 2 * cast_int_from_bool(R(1, 0) - R(0, 1) < 0);
  return {sqrt(a) / 4, sign_21 * sqrt(b) / 4, sign_02 * sqrt(c) / 4, sign_10 * sqrt(d) / 4};
}

MatrixExpr Quaternion::jacobian(const math::MatrixExpr& vars) const {
  if (vars.rows() != 1 && vars.cols() != 1) {
    throw DimensionError("Variables must be a row or column vector. Received dimensions: [{}, {}]",
                         vars.rows(), vars.cols());
  }
  const auto& m = vars.as_matrix();
  return jacobian(m.data());
}

MatrixExpr Quaternion::right_retract_derivative() const {
  // Compute the expression `J` once, and substitute into it on subsequent calls:
  static const Quaternion q_sub{
      make_unique_variable_symbol(NumberSet::Real), make_unique_variable_symbol(NumberSet::Real),
      make_unique_variable_symbol(NumberSet::Real), make_unique_variable_symbol(NumberSet::Real)};
  static const auto J = std::invoke([&]() -> MatrixExpr {
    const auto vx = make_unique_variable_symbol(NumberSet::Real);
    const auto vy = make_unique_variable_symbol(NumberSet::Real);
    const auto vz = make_unique_variable_symbol(NumberSet::Real);
    const Quaternion q_perturb = q_sub * Quaternion::from_rotation_vector(vx, vy, vz, 0);
    // Compute the Jacobian about 0:
    // clang-format off
    return substitute_variables(
        q_perturb.jacobian({vx, vy, vz}),
        {
            std::make_tuple(vx, Constants::Zero),
            std::make_tuple(vy, Constants::Zero),
            std::make_tuple(vz, Constants::Zero)
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

MatrixExpr Quaternion::right_local_coordinates_derivative() const {
  static const Quaternion q_sub{
      make_unique_variable_symbol(NumberSet::Real), make_unique_variable_symbol(NumberSet::Real),
      make_unique_variable_symbol(NumberSet::Real), make_unique_variable_symbol(NumberSet::Real)};
  static const auto J = std::invoke([&]() -> MatrixExpr {
    const auto dw = make_unique_variable_symbol(NumberSet::Real);
    const auto dx = make_unique_variable_symbol(NumberSet::Real);
    const auto dy = make_unique_variable_symbol(NumberSet::Real);
    const auto dz = make_unique_variable_symbol(NumberSet::Real);
    const Quaternion q_diff = q_sub.conjugate() * Quaternion{q_sub.w() + dw, q_sub.x() + dx,
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

Quaternion operator*(const Quaternion& a, const Quaternion& b) {
  return {a.w() * b.w() - a.x() * b.x() - a.y() * b.y() - a.z() * b.z(),
          a.w() * b.x() + a.x() * b.w() + a.y() * b.z() - a.z() * b.y(),
          a.w() * b.y() + a.y() * b.w() + a.z() * b.x() - a.x() * b.z(),
          a.w() * b.z() + a.z() * b.w() + a.x() * b.y() - a.y() * b.x()};
}

}  // namespace math
