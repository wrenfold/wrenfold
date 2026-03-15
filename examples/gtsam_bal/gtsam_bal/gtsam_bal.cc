// Example of integrating wrenfold-generated factors into GTSAM.
// This is based on the `SFMExample_bal.cpp` from GTSAM, with modifications.
// This example loads a dataset from the BAL dataset, and optimizes it with levenberg marquardt.
#include <optional>
#include <string_view>

#include <fmt/core.h>

#include <gtsam/base/OptionalJacobian.h>
#include <gtsam/inference/Symbol.h>
#include <gtsam/linear/NoiseModel.h>
#include <gtsam/nonlinear/Expression.h>
#include <gtsam/nonlinear/ExpressionFactor.h>
#include <gtsam/nonlinear/LevenbergMarquardtOptimizer.h>
#include <gtsam/nonlinear/NonlinearFactorGraph.h>
#include <gtsam/nonlinear/expressions.h>
#include <gtsam/sfm/SfmData.h>
#include <gtsam/sfm/SfmTrack.h>
#include <gtsam/slam/GeneralSFMFactor.h>
#include <gtsam/slam/dataset.h>

#include <wrenfold/span.h>

#include "generated/bundle_adjustment_factor.h"
#include "gtsam/geometry/CalibratedCamera.h"

// Wrap the generated function in a GTSAM-friendly interface.
// This function accepts the SFM camera and point location in world, and produces pixel coordinates.
gtsam::Vector2 bundle_adjustment_factor(const gtsam::SfmCamera& camera, const gtsam::Vector3& point,
                                        gtsam::OptionalJacobian<2, 9> D_camera,
                                        gtsam::OptionalJacobian<2, 3> D_point) {
  gtsam::Vector2 p_image;
  gen::bundle_adjustment_factor<double>(
      camera, point, p_image,
      D_camera ? *D_camera : Eigen::Map<gtsam::OptionalJacobian<2, 9>::Jacobian>(nullptr),
      D_point ? *D_point : Eigen::Map<gtsam::OptionalJacobian<2, 3>::Jacobian>(nullptr));
  return p_image;
}

namespace handwritten {
// For comparison, a handwritten implementation of the same factor that executes the different
// steps and then chain rules them together.
gtsam::Vector2 bundle_adjustment_factor(const gtsam::SfmCamera& camera, const gtsam::Vector3& point,
                                        gtsam::OptionalJacobian<2, 9> D_camera,
                                        gtsam::OptionalJacobian<2, 3> D_point) {
  gtsam::Matrix36 p_camera_D_pose;
  gtsam::Matrix33 p_camera_D_point;
  const gtsam::Vector3 p_camera = camera.pose().transformTo(
      point, D_camera ? &p_camera_D_pose : nullptr, D_point ? &p_camera_D_point : nullptr);

  gtsam::Matrix23 p_unit_depth_D_p_camera;
  const gtsam::Vector2 p_unit_depth = gtsam::PinholeBase::Project(
      p_camera, D_camera || D_point ? &p_unit_depth_D_p_camera : nullptr);

  gtsam::Matrix23 p_image_D_cal;
  gtsam::Matrix22 p_image_D_p_unit_depth;
  const gtsam::Vector2 p_image =
      camera.calibration().uncalibrate(p_unit_depth, D_camera ? &p_image_D_cal : nullptr,
                                       D_camera || D_point ? &p_image_D_p_unit_depth : nullptr);

  if (D_camera) {
    D_camera->leftCols<6>() = p_image_D_p_unit_depth * p_unit_depth_D_p_camera * p_camera_D_pose;
    D_camera->rightCols<3>() = p_image_D_cal;
  }
  if (D_point) {
    D_point->noalias() = p_image_D_p_unit_depth * p_unit_depth_D_p_camera * p_camera_D_point;
  }
  return p_image;
}
}  // namespace handwritten

enum class factor_mode {
  // Use symbolic code-generated factor.
  symbolic = 0,
  // Use the `GeneralSFMFactor`.
  sfm,
  // Use handwritten chain-rule version.
  handwritten,
};

inline constexpr std::optional<factor_mode> parse_factor_mode(const std::string_view mode) {
  if (mode == "symbolic") {
    return factor_mode::symbolic;
  } else if (mode == "sfm") {
    return factor_mode::sfm;
  } else if (mode == "handwritten") {
    return factor_mode::handwritten;
  }
  return std::nullopt;
}

int main(const int argc, char* argv[]) {
  if (argc < 2) {
    fmt::print("Please supply a path to the BAL dataset.\n");
    return 1;
  }

  const std::optional<factor_mode> mode =
      argc > 2 ? parse_factor_mode(argv[2]) : factor_mode::symbolic;
  if (!mode) {
    fmt::print("Invalid factor mode specified!\n");
    return 1;
  }

  const std::string filename{argv[1]};
  fmt::print("Loading BAL data from: {}\n", filename);

  const gtsam::SfmData sfm_data = gtsam::SfmData::FromBalFile(filename);
  fmt::print("Read {} tracks on {} cameras.\n", sfm_data.numberTracks(), sfm_data.numberCameras());

  // Isotropic noise model w/ 1 pixel error.
  const auto noise_model = gtsam::noiseModel::Isotropic::Sigma(2, 1.0);

  using gtsam::symbol_shorthand::C;
  using gtsam::symbol_shorthand::P;

  // Build the graph of projection factors:
  gtsam::NonlinearFactorGraph graph{};
  std::size_t track_index = 0;
  for (const gtsam::SfmTrack& track : sfm_data.tracks) {
    const gtsam::Vector3_ p_world(P(track_index));
    for (const gtsam::SfmMeasurement& m : track.measurements) {
      // To allow comparing the errors for different solutions, we can toggle between different
      // implementations.
      if (mode.value() == factor_mode::symbolic) {
        // Use the code-generated symbolic factor:
        const gtsam::Expression<gtsam::Vector2> p_image(
            bundle_adjustment_factor, gtsam::Expression<gtsam::SfmCamera>(C(m.first)), p_world);
        graph.addExpressionFactor<gtsam::Vector2>(noise_model, m.second, p_image);
      } else if (mode.value() == factor_mode::sfm) {
        // Use the built-in factor:
        graph.emplace_shared<gtsam::GeneralSFMFactor<gtsam::SfmCamera, gtsam::Vector3>>(
            m.second, noise_model, C(m.first), P(track_index));
      } else {
        // Use `handwritten::bundle_adjustment_factor`.
        const gtsam::Expression<gtsam::Vector2> p_image(
            handwritten::bundle_adjustment_factor, gtsam::Expression<gtsam::SfmCamera>(C(m.first)),
            p_world);
        graph.addExpressionFactor<gtsam::Vector2>(noise_model, m.second, p_image);
      }
    }
    ++track_index;
  }

  // As in SFMExample_bal.cpp, add priors to constrain scale:
  graph.addPrior(C(0), sfm_data.cameras[0], gtsam::noiseModel::Isotropic::Sigma(9, 0.1));
  graph.addPrior(P(0), sfm_data.tracks[0].p, gtsam::noiseModel::Isotropic::Sigma(3, 0.1));

  // Create initial estimate
  gtsam::Values initial;
  std::size_t camera_index = 0;
  track_index = 0;
  for (const gtsam::SfmCamera& camera : sfm_data.cameras) {
    initial.insert(C(camera_index), camera);
    ++camera_index;
  }
  for (const gtsam::SfmTrack& track : sfm_data.tracks) {
    initial.insert(P(track_index), track.p);
    ++track_index;
  }

  // Optimize
  gtsam::Values result;
  try {
    gtsam::LevenbergMarquardtParams params;
    params.setMaxIterations(40);
    params.setVerbosity("ERROR");
    gtsam::LevenbergMarquardtOptimizer lm(graph, initial, params);
    result = lm.optimize();
  } catch (const std::exception& e) {
    fmt::print("Error while optimizing: {}\n", e.what());
    return 2;
  }
  fmt::print("Final error: {}\n", graph.error(result));
  return 0;
}
