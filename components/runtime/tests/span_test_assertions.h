// Copyright 2023 Gareth Cross
#include <fmt/format.h>
#include <gtest/gtest.h>

#define EXPECT_EIGEN_SPAN_EQ(a, b) EXPECT_PRED_FORMAT2(wf::detail::expect_eigen_span_equal, a, b)
#define ASSERT_EIGEN_SPAN_EQ(a, b) ASSERT_PRED_FORMAT2(wf::detail::expect_eigen_span_equal, a, b)

// Define an assertion so we can test that functionality:
#define MATH_SPAN_RUNTIME_ASSERT(condition)    \
  do {                                         \
    if (!static_cast<bool>(condition)) {       \
      throw std::runtime_error("Test assert"); \
    }                                          \
  } while (false)

#include "wf_runtime/span.h"
#include "wf_runtime/span_eigen.h"

namespace wf {
namespace detail {

// Compare eigen matrix and span. Use ASSERT_EIGEN_SPAN_EQ()
template <typename Derived, typename Scalar, typename Dimensions, typename Strides>
testing::AssertionResult expect_eigen_span_equal(const std::string& name_a,
                                                 const std::string& name_b,
                                                 const Eigen::MatrixBase<Derived>& a,
                                                 const wf::span<Scalar, Dimensions, Strides> b) {
  if (b.data() == nullptr) {
    return testing::AssertionFailure() << fmt::format("Span expression has null data: {} ", name_b);
  }
  if (a.rows() != static_cast<Eigen::Index>(b.rows()) ||
      a.cols() != static_cast<Eigen::Index>(b.cols())) {
    return testing::AssertionFailure()
           << fmt::format("Dimensions of {}: [{}, {}] and {}: [{}, {}] do not match.", name_a,
                          a.rows(), a.cols(), name_b, b.rows(), b.cols());
  }
  for (int i = 0; i < a.rows(); ++i) {
    for (int j = 0; j < a.cols(); ++j) {
      if (a(i, j) != b(i, j)) {
        return testing::AssertionFailure() << fmt::format(
                   "Matrix equality {} == {} failed because:\n"
                   "{}({}, {}) is {} and {}({}, {}) is {}.\n",
                   name_a, name_b, name_a, i, j, a(i, j), name_b, i, j, b(i, j));
      }
    }
  }
  return testing::AssertionSuccess();
}

}  // namespace detail
}  // namespace wf
