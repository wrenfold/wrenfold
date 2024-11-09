// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numerical_jacobian.h"

#include "generated.h"

namespace wf {

// Test coefficients for 4 different types of cameras.
// We return tuples of (coefficients, max_image_plane_radius).
const auto& get_fisheye_coefficients() {
  static const std::vector<std::tuple<Eigen::Vector4d, double>> coeffs = {
      // Stereographic
      std::make_tuple(Eigen::Vector4d{0.08327424, 0.00852979, 0.00063325, 0.00017048}, 1.99),
      // Equidistant.
      std::make_tuple(Eigen::Vector4d{0.0, 0.0, 0.0, 0.0}, M_PI / 2),
      // Equisolid.
      std::make_tuple(
          Eigen::Vector4d{-4.16666666e-02, 5.20833056e-04, -3.09988000e-06, 1.06158708e-08}, 1.41),
      // Orthogonal.
      std::make_tuple(
          Eigen::Vector4d{-1.66666587e-01, 8.33305775e-03, -1.98095183e-04, 2.60641004e-06}, 0.99),
  };
  return coeffs;
}

template <typename Scalar, int Rows, int Cols>
input_span2d_t input_span_from_eigen(const Eigen::Matrix<Scalar, Rows, Cols>& v) {
  return input_span2d_t{v.data(), static_cast<uint32_t>(v.rows()), static_cast<uint32_t>(v.cols()),
                        static_cast<uint32_t>(v.outerStride())};
}

template <typename Scalar, int Rows, int Cols>
output_span2d_t output_span_from_eigen(Eigen::Matrix<Scalar, Rows, Cols>& v) {
  return output_span2d_t{v.data(), static_cast<uint32_t>(v.rows()), static_cast<uint32_t>(v.cols()),
                         static_cast<uint32_t>(v.outerStride())};
}

// Test round-trip un-projection --> projection with the Kannala-Brandt (KB) camera model.
TEST(CGenerationTest, TestKbCameraRoundTrip) {
  constexpr double cx = (320.0 - 1) / 2.0;
  constexpr double cy = (240.0 - 1) / 2.0;
  const double pixel_radius = std::sqrt(std::pow(cx, 2) + std::pow(cy, 2));

  for (const auto& [coeffs, max_radius] : get_fisheye_coefficients()) {
    const double f = pixel_radius / max_radius;
    const Eigen::Vector4d K{f * 1.05, f * 1.03, cx, cy};

    for (int y = 0; y < 240; y += 10) {
      for (int x = 0; x < 320; x += 10) {
        const Eigen::Vector2d p_pixels{static_cast<double>(x), static_cast<double>(y)};

        Eigen::Vector3d p_cam;
        kb_camera_unprojection_with_jacobians(
            input_span_from_eigen(p_pixels), input_span_from_eigen(K),
            input_span_from_eigen(coeffs), output_span_from_eigen(p_cam), nullptr, nullptr,
            nullptr);

        Eigen::Vector2d p_pixels_out;
        kb_camera_projection_with_jacobians(
            input_span_from_eigen(p_cam), input_span_from_eigen(K), input_span_from_eigen(coeffs),
            output_span_from_eigen(p_pixels_out), nullptr, nullptr, nullptr);

        ASSERT_EIGEN_NEAR(p_pixels, p_pixels_out, 1.0e-4);
      }
    }
  }
}

// Test jacobians of the KB camera model projection.
TEST(CGenerationTest, TestKbCameraProjectJacobian) {
  constexpr double cx = (320.0 - 1) / 2.0;
  constexpr double cy = (240.0 - 1) / 2.0;
  const double pixel_radius = std::sqrt(std::pow(cx, 2) + std::pow(cy, 2));

  Eigen::Matrix<double, 2, 3> p_pixels_D_p_cam;
  Eigen::Matrix<double, 2, 4> p_pixels_D_K;
  Eigen::Matrix<double, 2, 4> p_pixels_D_coeffs;

  auto p_pixels_D_p_cam_span = output_span_from_eigen(p_pixels_D_p_cam);
  auto p_pixels_D_K_span = output_span_from_eigen(p_pixels_D_K);
  auto p_pixels_D_coeffs_span = output_span_from_eigen(p_pixels_D_coeffs);

  for (const auto& [coeffs, max_radius] : get_fisheye_coefficients()) {
    const double f = pixel_radius / max_radius;
    const Eigen::Vector4d K{f * 1.05, f * 1.03, cx, cy};

    // Iterate over a bunch of unit vectors in camera frame:
    for (double theta = 0.01; theta < M_PI / 2; theta += 0.1) {
      for (double phi = -M_PI; phi < M_PI; phi += 0.1) {
        const Eigen::Vector3d p_cam{std::cos(phi) * std::sin(theta),
                                    std::sin(phi) * std::sin(theta), std::cos(theta)};

        Eigen::Vector2d p_pixels_out;
        kb_camera_projection_with_jacobians(
            input_span_from_eigen(p_cam), input_span_from_eigen(K), input_span_from_eigen(coeffs),
            output_span_from_eigen(p_pixels_out), &p_pixels_D_p_cam_span, &p_pixels_D_K_span,
            &p_pixels_D_coeffs_span);

        // Compute derivative wrt p_cam numerically:
        const auto p_pixels_D_p_cam_num =
            numerical_jacobian(p_cam, [&](const Eigen::Vector3d& p_cam_perturbed) {
              Eigen::Vector2d p_pixels_out;
              kb_camera_projection_with_jacobians(
                  input_span_from_eigen(p_cam_perturbed), input_span_from_eigen(K),
                  input_span_from_eigen(coeffs), output_span_from_eigen(p_pixels_out), nullptr,
                  nullptr, nullptr);
              return p_pixels_out;
            });
        ASSERT_EIGEN_NEAR(p_pixels_D_p_cam_num, p_pixels_D_p_cam, 1.0e-8);

        // Compute derivative wrt K numerically:
        const auto p_pixels_D_K_num =
            numerical_jacobian(K, [&](const Eigen::Vector4d& K_perturbed) {
              Eigen::Vector2d p_pixels_out;
              kb_camera_projection_with_jacobians(
                  input_span_from_eigen(p_cam), input_span_from_eigen(K_perturbed),
                  input_span_from_eigen(coeffs), output_span_from_eigen(p_pixels_out), nullptr,
                  nullptr, nullptr);
              return p_pixels_out;
            });
        ASSERT_EIGEN_NEAR(p_pixels_D_K_num, p_pixels_D_K, 1.0e-8);

        // Compute derivative wrt coeffs numerically:
        const auto p_pixels_D_coeffs_num =
            numerical_jacobian(coeffs, [&](const Eigen::Vector4d& coeffs_perturbed) {
              Eigen::Vector2d p_pixels_out;
              kb_camera_projection_with_jacobians(
                  input_span_from_eigen(p_cam), input_span_from_eigen(K),
                  input_span_from_eigen(coeffs_perturbed), output_span_from_eigen(p_pixels_out),
                  nullptr, nullptr, nullptr);
              return p_pixels_out;
            });
        ASSERT_EIGEN_NEAR(p_pixels_D_coeffs_num, p_pixels_D_coeffs, 1.0e-8);
      }
    }
  }
}

TEST(CGenerationTest, TestKbCameraUnprojectJacobian) {
  constexpr double cx = (320.0 - 1) / 2.0;
  constexpr double cy = (240.0 - 1) / 2.0;
  const double pixel_radius = std::sqrt(std::pow(cx, 2) + std::pow(cy, 2));

  Eigen::Matrix<double, 3, 2> p_cam_D_p_pixels;
  Eigen::Matrix<double, 3, 4> p_cam_D_K;
  Eigen::Matrix<double, 3, 4> p_cam_D_coeffs;

  auto p_cam_D_p_pixels_span = output_span_from_eigen(p_cam_D_p_pixels);
  auto p_cam_D_K_span = output_span_from_eigen(p_cam_D_K);
  auto p_cam_D_coeffs_span = output_span_from_eigen(p_cam_D_coeffs);

  for (const auto& [coeffs, max_radius] : get_fisheye_coefficients()) {
    const double f = pixel_radius / max_radius;
    const Eigen::Vector4d K{f * 1.05, f * 1.03, cx, cy};

    // Iterate over a bunch of pixel coordinates:
    for (int y = 0; y < 240; y += 10) {
      for (int x = 0; x < 320; x += 10) {
        const Eigen::Vector2d p_pixels{static_cast<double>(x), static_cast<double>(y)};

        Eigen::Vector3d p_cam_out;
        kb_camera_unprojection_with_jacobians(
            input_span_from_eigen(p_pixels), input_span_from_eigen(K),
            input_span_from_eigen(coeffs), output_span_from_eigen(p_cam_out),
            &p_cam_D_p_pixels_span, &p_cam_D_K_span, &p_cam_D_coeffs_span);

        // Compute derivative wrt p_pixels:
        const auto p_cam_D_p_pixels_num =
            numerical_jacobian(p_pixels, [&](const Eigen::Vector2d& p_pixels_perturbed) {
              Eigen::Vector3d p_cam_out;
              kb_camera_unprojection_with_jacobians(
                  input_span_from_eigen(p_pixels_perturbed), input_span_from_eigen(K),
                  input_span_from_eigen(coeffs), output_span_from_eigen(p_cam_out), nullptr,
                  nullptr, nullptr);
              return p_cam_out;
            });
        ASSERT_EIGEN_NEAR(p_cam_D_p_pixels_num, p_cam_D_p_pixels, 1.0e-8);

        // Compute derivative wrt K:
        const auto p_cam_D_K_num = numerical_jacobian(K, [&](const Eigen::Vector4d& K_perturbed) {
          Eigen::Vector3d p_cam_out;
          kb_camera_unprojection_with_jacobians(
              input_span_from_eigen(p_pixels), input_span_from_eigen(K_perturbed),
              input_span_from_eigen(coeffs), output_span_from_eigen(p_cam_out), nullptr, nullptr,
              nullptr);
          return p_cam_out;
        });
        ASSERT_EIGEN_NEAR(p_cam_D_K_num, p_cam_D_K, 1.0e-8);

        // Compute derivative wrt K.
        // Use a much smaller step size here to accommodate the solver.
        const auto p_cam_D_coeffs_num = numerical_jacobian(
            coeffs,
            [&](const Eigen::Vector4d& coeffs_perturbed) {
              Eigen::Vector3d p_cam_out;
              kb_camera_unprojection_with_jacobians(
                  input_span_from_eigen(p_pixels), input_span_from_eigen(K),
                  input_span_from_eigen(coeffs_perturbed), output_span_from_eigen(p_cam_out),
                  nullptr, nullptr, nullptr);
              return p_cam_out;
            },
            1.0e-5);
        ASSERT_EIGEN_NEAR(p_cam_D_coeffs_num, p_cam_D_coeffs, 1.0e-6);
      }
    }
  }
}

// Flip the sign of `actual` to match `ref` (if they do not match).
void flip_sign_to_match(const Eigen::Quaterniond& ref, quaternion_t& actual) {
  // Take the first non-zero element:
  for (const auto& [r, a] :
       {std::make_tuple(ref.w(), actual.w), std::make_tuple(ref.x(), actual.x),
        std::make_tuple(ref.y(), actual.y), std::make_tuple(ref.z(), actual.z)}) {
    if (r != 0) {
      if (std::signbit(r) != std::signbit(a)) {
        actual.w *= -1;
        actual.x *= -1;
        actual.y *= -1;
        actual.z *= -1;
      }
      break;
    }
  }
}

TEST(CGenerationTest, TestQuaternionFromMatrix) {
  // Some edge cases and random quaternions.
  const std::vector<Eigen::Quaterniond> qs_edge = {
      {1.0, 0.0, 0.0, 0.0},
      {-1.0, 0.0, 0.0, 0.0},
      {0, 1 / std::sqrt(2.0), 1 / std::sqrt(2.0), 0},
      {0, -1 / std::sqrt(2.0), 1 / std::sqrt(2.0), 0},
      {0, 1 / std::sqrt(2.0), -1 / std::sqrt(2.0), 0},
      {0, 0, 1 / std::sqrt(2.0), 1 / std::sqrt(2.0)},
      {0, 0, -1 / std::sqrt(2.0), 1 / std::sqrt(2.0)},
      {0, 0, 1 / std::sqrt(2.0), -1 / std::sqrt(2.0)},
      {1 / std::sqrt(2.0), 1 / std::sqrt(2.0), 0, 0},
      {-1 / std::sqrt(2.0), 1 / std::sqrt(2.0), 0, 0},
      {1 / std::sqrt(2.0), -1 / std::sqrt(2.0), 0, 0},
  };

  const std::vector<Eigen::Quaterniond> qs_random = {
      {-0.51879141, -0.43584291, 0.68075724, 0.27832716},
      {0.40285343, 0.49457606, -0.65747589, 0.40103503},
      {0.20803629, -0.15572734, -0.56428821, 0.7836126},
      {0.80776844, -0.16735421, 0.49527993, -0.2723977},
      {0.29067337, -0.27204595, 0.76530082, -0.50578122},
      {0.64262622, 0.03507511, 0.37740948, -0.66585536},
      {-0.57799396, 0.0972896, -0.56068941, -0.58488042},
      {-0.40712309, -0.79537291, 0.28588786, 0.34626703},
      {0.55416343, -0.42971437, -0.15799486, 0.6951878},
      {0.14457519, -0.48679597, -0.86128872, 0.0175908}};

  for (const auto& q : qs_edge) {
    const Eigen::Matrix3d R = q.toRotationMatrix();
    quaternion_t q_recovered = quaternion_from_rotation_matrix(input_span_from_eigen(R));

    flip_sign_to_match(q, q_recovered);
    ASSERT_NEAR(q.w(), q_recovered.w, 1.0e-12);
    ASSERT_NEAR(q.x(), q_recovered.x, 1.0e-12);
    ASSERT_NEAR(q.y(), q_recovered.y, 1.0e-12);
    ASSERT_NEAR(q.z(), q_recovered.z, 1.0e-12);
  }

  for (const auto& q : qs_random) {
    const Eigen::Matrix3d R = q.toRotationMatrix();
    quaternion_t q_recovered = quaternion_from_rotation_matrix(input_span_from_eigen(R));

    flip_sign_to_match(q, q_recovered);
    ASSERT_NEAR(q.w(), q_recovered.w, 1.0e-8);
    ASSERT_NEAR(q.x(), q_recovered.x, 1.0e-8);
    ASSERT_NEAR(q.y(), q_recovered.y, 1.0e-8);
    ASSERT_NEAR(q.z(), q_recovered.z, 1.0e-8);
  }
}

}  // namespace wf
