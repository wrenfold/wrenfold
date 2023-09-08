// Copyright 2023 Gareth Cross
#include <fmt/format.h>
#include <gtest/gtest.h>

#define MATH_SPAN_EIGEN_SUPPORT
#include "span.h"

#define EXPECT_EIGEN_SPAN_EQ(a, b) EXPECT_PRED_FORMAT2(math::detail::ExpectEigenSpanEqual, a, b)
#define ASSERT_EIGEN_SPAN_EQ(a, b) ASSERT_PRED_FORMAT2(math ::detail::ExpectEigenSpanEqual, a, b)

namespace math {
namespace detail {
// Compare eigen matrix and span. Use ASSERT_EIGEN_SPAN_EQ()
// Implementation defined below.
template <typename Ta, typename Scalar, std::size_t R, std::size_t C, typename RowStride,
          typename ColStride>
testing::AssertionResult ExpectEigenSpanEqual(
    const std::string& name_a, const std::string& name_b, const Eigen::MatrixBase<Ta>& a,
    const math::Span<Scalar, R, C, RowStride, ColStride> b);
}  // namespace detail

TEST(SpanTest, TestStaticIndexing) {
  Span<float, 3, 3, Const<3>, Const<1>> row_major{nullptr, {}, {}};
  ASSERT_EQ(sizeof(void*), sizeof(row_major));  //  Should be same size as a pointer.
  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(1, row_major.compute_index(0, 1));
  ASSERT_EQ(3, row_major.compute_index(1, 0));
  ASSERT_EQ(5, row_major.compute_index(1, 2));

  Span<float, 3, 3, Const<1>, Const<4>> col_major{nullptr, {}, {}};
  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(4, col_major.compute_index(0, 1));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(6, col_major.compute_index(2, 1));

  Span<float, 3, 3, Const<3>, Const<9>> both{nullptr, {}, {}};
  ASSERT_EQ(0, both.compute_index(0, 0));
  ASSERT_EQ(9, both.compute_index(0, 1));
  ASSERT_EQ(3, both.compute_index(1, 0));
  ASSERT_EQ(15, both.compute_index(2, 1));
  ASSERT_EQ(24, both.compute_index(2, 2));
}

TEST(SpanTest, TestDynamicIndexing) {
  Span<float, 3, 4, Dynamic, Const<1>> row_major{nullptr, Dynamic(8), {}};
  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(8, row_major.compute_index(1, 0));
  ASSERT_EQ(11, row_major.compute_index(1, 3));
  ASSERT_EQ(17, row_major.compute_index(2, 1));

  Span<float, 3, 4, Const<1>, Dynamic> col_major{nullptr, {}, Dynamic(6)};
  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(8, col_major.compute_index(2, 1));
}

TEST(SpanTest, TestConstructor) {
  std::array<int, 6> data = {0, 1, 2, 3, 4, 5};
  auto span = Span<int, 3, 2, Const<1>, Const<3>>{data.data(), {}, {}};
  EXPECT_EQ(3, span.rows());
  EXPECT_EQ(2, span.cols());
  EXPECT_EQ(1, span.stride<0>().value());
  EXPECT_EQ(3, span.stride<1>().value());
  EXPECT_EQ(0, span(0, 0));
  EXPECT_EQ(1, span(1, 0));
  EXPECT_EQ(3, span(0, 1));

  // Implicit conversion to const:
  auto const_span = Span<int, 3, 2, Const<1>, Const<3>>{span};
  EXPECT_EQ(span.data(), const_span.data());
  EXPECT_EQ(span, const_span);
}

TEST(SpanTest, TestEqualityOperator) {
  const std::array<int, 4> data1 = {0, 1, 2, 3};
  std::array<int, 4> data2 = {4, 5, 6, 7};

  auto span1 = Span<const int, 2, 2, Const<1>, Const<2>>{data1.data(), {}, {}};
  EXPECT_EQ(span1, span1);
  EXPECT_EQ(span1, span1.with_dynamic_strides());

  auto span2 = Span<int, 2, 2, Const<2>, Const<1>>{data2.data(), {}, {}};
  EXPECT_EQ(span2, span2.with_dynamic_strides());
}

TEST(SpanTest, TestEigenColMajor) {
  const Eigen::Matrix<int, 2, 3> A = (Eigen::Matrix<int, 2, 3>() << 1, 2, 3, 4, 5, 6).finished();
  EXPECT_EQ(1, A.innerStride());
  EXPECT_EQ(2, A.outerStride());

  auto span = make_span_eigen(A);
  static_assert(1 == span.stride<0>().value(), "Should be computable at compile time");
  static_assert(2 == span.stride<1>().value(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(A, span);
}

TEST(SpanTest, TestEigenRowMajor) {
  const Eigen::Matrix<int, 4, 3, Eigen::RowMajor> B =
      (Eigen::Matrix<int, 4, 3>() << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).finished();
  EXPECT_EQ(1, B.innerStride());
  EXPECT_EQ(3, B.outerStride());

  auto span = make_span_eigen(B);
  static_assert(3 == span.stride<0>().value(), "Should be computable at compile time");
  static_assert(1 == span.stride<1>().value(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(B, span);
}

TEST(SpanTest, TestEigenColMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 5, 6>() << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                             13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
                                .finished();

  auto blk_1 = A.topRightCorner<2, 2>();
  auto span_1 = make_span_eigen(blk_1);
  ASSERT_EQ(1, span_1.stride<0>().value());
  ASSERT_EQ(5, span_1.stride<1>().value());
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  auto blk_2 = A.bottomRightCorner<3, 4>();
  auto span_2 = make_span_eigen(blk_2);
  ASSERT_EQ(1, span_2.stride<0>().value());
  ASSERT_EQ(5, span_2.stride<1>().value());
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);

  auto blk_3 = A.middleCols<3>(2).middleRows<3>(1);
  auto span_3 = make_span_eigen(blk_3);
  EXPECT_EIGEN_SPAN_EQ(blk_3, span_3);
}

TEST(SpanTest, TestEigenRowMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 3, 7, Eigen::RowMajor>() << 1, 2, 3, 4, 5, 6, 7, 8,
                             9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
                                .finished();

  auto blk_1 = A.block<3, 3>(0, 2);
  auto span_1 = make_span_eigen(blk_1);
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  auto blk_2 = A.bottomRightCorner<2, 3>();
  auto span_2 = make_span_eigen(blk_2);
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);
}

TEST(SpanTest, TestEigenMap) {
  const std::array<int, 9> data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  const Eigen::Map<const Eigen::Matrix3i, Eigen::Unaligned> map{data.data()};
  auto span = make_span_eigen(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
}

// In the tests below, we fill elements that should be skipped by the strides w/ zero.
// Then we check that our span does not include any zero elements.
template <std::size_t Rows, std::size_t Cols, typename... Strides>
void CheckNonZero(math::Span<const int, Rows, Cols, Strides...> span) {
  for (int i = 0; i < span.rows(); ++i) {
    for (int j = 0; j < span.cols(); ++j) {
      ASSERT_NE(0, span(i, j));
    }
  }
}

TEST(SpanTest, TestEigenMapInnerStride) {
  const std::array<int, 39> data = {1,  0, 2,  0, 3,  0,  4,  0,  5,  0,  6,  0,  7,
                                    0,  8, 0,  9, 0,  10, 0,  11, 0,  12, 0,  13, 0,
                                    14, 0, 15, 0, 16, 0,  17, 0,  18, 0,  19, 0,  20};

  const Eigen::Map<const Eigen::Matrix<int, 5, 4, Eigen::RowMajor>, Eigen::Unaligned,
                   Eigen::InnerStride<2>>
      map{data.data()};
  auto span = make_span_eigen(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 5, 4, Eigen::RowMajor>, Eigen::Unaligned, DynamicStride>
      map_dynamic{data.data(), DynamicStride{8, 2}};
  auto span_dynamic = make_span_eigen(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

TEST(SpanTest, TestEigenMapOuterStride) {
  const std::array<int, 23> data = {1,  6, 11, 0, 0, 2,  7, 12, 0, 0,  3, 8,
                                    13, 0, 0,  4, 9, 14, 0, 0,  5, 10, 15};

  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, Eigen::OuterStride<5>> map{
      data.data()};
  auto span = make_span_eigen(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{5, 1}};

  auto span_dynamic = make_span_eigen(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

TEST(SpanTest, TestEigenMapInnerAndOuterStride) {
  const std::array<int, 29> data = {1, 0, 4, 0, 7, 0, 0, 0, 0, 0, 0, 0, 2, 0, 5,
                                    0, 8, 0, 0, 0, 0, 0, 0, 0, 3, 0, 6, 0, 9};

  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, Eigen::Stride<12, 2>> map{
      data.data()};
  auto span = make_span_eigen(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  EXPECT_EQ(1, span(0, 0));
  EXPECT_EQ(4, span(1, 0));
  EXPECT_EQ(2, span(0, 1));
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{12, 2}};

  auto span_dynamic = make_span_eigen(map_dynamic);
  static_assert(sizeof(span_dynamic) == 24, "");

  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

namespace detail {
template <typename Ta, typename Scalar, std::size_t R, std::size_t C, typename RowStride,
          typename ColStride>
testing::AssertionResult ExpectEigenSpanEqual(
    const std::string& name_a, const std::string& name_b, const Eigen::MatrixBase<Ta>& a,
    const math::Span<Scalar, R, C, RowStride, ColStride> b) {
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

}  // namespace math
