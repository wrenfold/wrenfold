// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <Eigen/Core>

#include "wf/utility/traits.h"

// Declare custom numeric types before importing the generated code:
namespace wf::numeric {
struct Point2d {
  double x;
  double y;

  Eigen::Vector2d to_vector() const { return {x, y}; }
};

struct Circle {
  Point2d center;
  double radius;

  Eigen::Vector3d to_vector() const { return {center.x, center.y, radius}; }
};

struct FancyAggregateType {
  Point2d pt;
  Circle circle;
  Eigen::Matrix<double, 2, 1> matrix;
  double scalar;

  Eigen::Matrix<double, 8, 1> to_vector() const {
    return (Eigen::Matrix<double, 8, 1>() << pt.x, pt.y, circle.center.x, circle.center.y,
            circle.radius, matrix, scalar)
        .finished();
  }
};

// An integer that can be implicitly cast to std::int64_t, and nothing else.
// We use this to make sure our generated code casts correctly when interfacing with
// `MixedNumerics`.
class type_safe_int64_t {
 public:
  template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
  constexpr type_safe_int64_t(T x) noexcept : x_(static_cast<std::int64_t>(x)) {}  //  NOLINT

  template <typename T, typename = enable_if_same_t<T, std::int64_t>>
  constexpr operator T() const noexcept {  //  NOLINT
    return x_;
  }

  // Allow explicit cast to double.
  explicit constexpr operator double() const noexcept { return static_cast<double>(x_); }

 private:
  std::int64_t x_;
};

struct MixedNumerics {
  double value;
  type_safe_int64_t mode;
};
}  // namespace wf::numeric
