// Copyright 2024 Gareth Cross
#include "wf_test_support/eigen_test_macros.h"
#include "wf_test_support/numeric_testing.h"
#include "wf_test_support/test_macros.h"

namespace types {
struct StructType {
  double x;
  double y;

  // Defined so we can compare to generate code.
  double dot(const StructType& other) const { return x * other.x + y * other.y; }

  // Linear interpolate from `this` to `other`.
  StructType lerp(const StructType& other, const double alpha) const {
    return StructType{other.x * alpha + (1 - alpha) * x, other.y * alpha + (1 - alpha) * y};
  }
};
}  // namespace types

namespace external {

// An external function that supports the generated code. Do a linear-interpolation lookup into
// the provided vector. In practice, we could also produce derivatives here and return an object
// that contains those as well.
types::StructType interpolate_access(const std::vector<types::StructType>& vec, const double x) {
  const double x_floor = std::floor(x);
  const double x_ceil = std::ceil(x);
  const double alpha = x - x_floor;

  const std::size_t i0 = static_cast<std::size_t>(x_floor);
  const std::size_t i1 = static_cast<std::size_t>(x_ceil);
  WF_ASSERT_LESS(i0, vec.size());
  WF_ASSERT_LESS(i1, vec.size());
  return vec[i0].lerp(vec[i1], alpha);
}

}  // namespace external

#include "generated.h"

namespace wf {

TEST(CppGenerationTest2, TestLookupAndComputeInnerProduct) {
  // Make up some test data - the exact generator function use here doesn't matter.
  constexpr std::size_t num_samples = 20;
  std::vector<types::StructType> test_vector{};
  for (std::size_t i = 0; i < num_samples; ++i) {
    const double theta = (static_cast<double>(i) + 0.5) / static_cast<double>(num_samples);
    test_vector.push_back(types::StructType{std::cos(theta), std::sin(theta)});
  }

  // Call our external method via our generated code to test that this works:
  ASSERT_NEAR(external::interpolate_access(test_vector, 0.0)
                  .dot(external::interpolate_access(test_vector, 19.0)),
              gen::lookup_and_compute_inner_product(test_vector, 0.0, 19.0), 1.0e-15);
  ASSERT_NEAR(external::interpolate_access(test_vector, 10.1)
                  .dot(external::interpolate_access(test_vector, 5.32)),
              gen::lookup_and_compute_inner_product(test_vector, 10.1, 5.32), 1.0e-15);
  ASSERT_NEAR(external::interpolate_access(test_vector, 0.123)
                  .dot(external::interpolate_access(test_vector, 18.01)),
              gen::lookup_and_compute_inner_product(test_vector, 0.123, 18.01), 1.0e-15);
  ASSERT_NEAR(external::interpolate_access(test_vector, 2.99)
                  .dot(external::interpolate_access(test_vector, 16.03)),
              gen::lookup_and_compute_inner_product(test_vector, 2.99, 16.03), 1.0e-15);
}

}  // namespace wf
