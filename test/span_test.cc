// Copyright 2023 Gareth Cross
#include <fmt/format.h>
#include <gtest/gtest.h>

// Define an assertion so we can test that functionality:
#define MATH_SPAN_RUNTIME_ASSERT(condition)    \
  do {                                         \
    if (!static_cast<bool>(condition)) {       \
      throw std::runtime_error("Test assert"); \
    }                                          \
  } while (false)

// We want to test eigen interop.
#define MATH_SPAN_EIGEN_SUPPORT

#include "span.h"
#include "span_eigen.h"

#define EXPECT_EIGEN_SPAN_EQ(a, b) EXPECT_PRED_FORMAT2(math::detail::ExpectEigenSpanEqual, a, b)
#define ASSERT_EIGEN_SPAN_EQ(a, b) ASSERT_PRED_FORMAT2(math ::detail::ExpectEigenSpanEqual, a, b)

namespace math {
namespace detail {
// Compare eigen matrix and span. Use ASSERT_EIGEN_SPAN_EQ()
// Implementation defined below.
template <typename Derived, typename Scalar, typename Dimensions, typename Strides>
testing::AssertionResult ExpectEigenSpanEqual(const std::string& name_a, const std::string& name_b,
                                              const Eigen::MatrixBase<Derived>& a,
                                              const math::span<Scalar, Dimensions, Strides> b);
}  // namespace detail

// Equality operator for spans of the same type.
// Defined for use in this test only for 2D matrices.
template <typename T, typename Dims1, typename Stride1, typename U, typename Dims2,
          typename Stride2, typename = detail::enable_if_same_after_removing_const_t<T, U>>
bool operator==(const span<T, Dims1, Stride1> a, const span<U, Dims2, Stride2> b) noexcept {
  static_assert(Dims1::length == Dims2::length && Dims1::length == 2, "Dims must be 2d.");
  if (static_cast<bool>(a) != static_cast<bool>(b)) {
    return false;
  }
  if (!a && !b) {
    return true;
  }
  if (a.rows() != b.rows() || a.cols() != b.cols()) {
    return false;
  }
  for (std::size_t i = 0; i < a.rows(); ++i) {
    for (std::size_t j = 0; j < a.cols(); ++j) {
      if (a(i, j) != b(i, j)) {
        return false;
      }
    }
  }
  return true;
}

TEST(SpanTest, TestStaticIndexing) {
  std::array<float, 9> unused_data{};

  auto row_major = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                             make_constant_value_pack<3, 1>());

  ASSERT_EQ(16, sizeof(row_major));
  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(1, row_major.compute_index(0, 1));
  ASSERT_EQ(3, row_major.compute_index(1, 0));
  ASSERT_EQ(5, row_major.compute_index(1, 2));

  auto col_major = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                             make_constant_value_pack<1, 4>());

  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(4, col_major.compute_index(0, 1));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(6, col_major.compute_index(2, 1));

  auto both = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                        make_constant_value_pack<3, 9>());

  ASSERT_EQ(0, both.compute_index(0, 0));
  ASSERT_EQ(9, both.compute_index(0, 1));
  ASSERT_EQ(3, both.compute_index(1, 0));
  ASSERT_EQ(15, both.compute_index(2, 1));
  ASSERT_EQ(24, both.compute_index(2, 2));
}

TEST(SpanTest, TestDynamicIndexing) {
  std::array<float, 12> unused_data{};

  auto row_major = make_span(unused_data.data(), make_constant_value_pack<3, 4>(),
                             make_value_pack(dynamic(8), constant<1>{}));

  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(8, row_major.compute_index(1, 0));
  ASSERT_EQ(11, row_major.compute_index(1, 3));
  ASSERT_EQ(17, row_major.compute_index(2, 1));

  auto col_major = make_span(unused_data.data(), make_constant_value_pack<3, 4>(),
                             make_value_pack(constant<1>{}, dynamic(6)));
  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(8, col_major.compute_index(2, 1));
}

TEST(SpanTest, TestConstructor) {
  std::array<int, 6> data = {0, 1, 2, 3, 4, 5};

  auto span =
      make_span(data.data(), make_constant_value_pack<3, 2>(), make_constant_value_pack<1, 3>());

  EXPECT_EQ(3, span.rows());
  EXPECT_EQ(2, span.cols());
  EXPECT_EQ(1, span.stride<0>());
  EXPECT_EQ(3, span.stride<1>());
  EXPECT_EQ(0, span.compute_index(0, 0));
  EXPECT_EQ(0, span(0, 0));
  EXPECT_EQ(1, span(1, 0));
  EXPECT_EQ(3, span(0, 1));

  auto const_span = span.as_const();
  static_assert(std::is_same<const int, decltype(const_span)::element_type>::value, "");
  EXPECT_EQ(span.data(), const_span.data());
  EXPECT_EQ(span, const_span);
}

TEST(SpanTest, TestMakeNullSpan) {
  auto span = make_always_null_span<float, 2, 3>();
  EXPECT_FALSE(span);
  EXPECT_EQ(nullptr, span.data());
  EXPECT_EQ(2, span.rows());
  EXPECT_EQ(3, span.cols());
  EXPECT_EQ(0, span.stride<0>());
  EXPECT_EQ(0, span.stride<1>());
}

TEST(SpanTest, TestMakeArraySpan) {
  std::array<int, 5> data = {1, 2, 3, 4, 5};
  auto span = make_array_span(data);
  EXPECT_EQ(data.data(), span.data());
  EXPECT_EQ(5, span.rows());
  EXPECT_EQ(1, span.stride<0>());
  for (int i = 0; i < data.size(); ++i) {
    EXPECT_EQ(i + 1, span[i]);
  }
}

TEST(SpanTest, TestMakeEmptyVectorSpan) {
  std::vector<int> data;
  auto span = make_array_span(static_cast<const std::vector<int>&>(data));
  EXPECT_EQ(nullptr, span.data());
  EXPECT_EQ(0, span.rows());
  EXPECT_EQ(1, span.stride<0>());
}

TEST(SpanTest, TestMakeVectorSpan) {
  std::vector<int> data = {5, 3, 8, 22};
  auto span = make_array_span(data);
  EXPECT_EQ(data.data(), span.data());
  EXPECT_EQ(data.size(), span.rows());
  EXPECT_EQ(1, span.stride<0>());
  for (std::size_t i = 0; i < data.size(); ++i) {
    EXPECT_EQ(data[i], span[i]);
  }
}

TEST(SpanTest, TestMakeCArraySpan) {
  int values[6] = {1, 1, 2, 3, 5, 8};
  auto span = make_array_span(values);

  static_assert(6 == span.rows(), "Size must be known at compile time");
  EXPECT_EQ(&values[0], span.data());
  for (std::size_t i = 0; i < span.rows(); ++i) {
    EXPECT_EQ(values[i], span[i]);
  }

  // Construction from const array:
  const int values_const[4] = {13, 21, 34, 55};
  auto span_const = make_array_span(values_const);
  static_assert(4 == span_const.rows(), "Size must be known at compile time");
  EXPECT_EQ(&values_const[0], span_const.data());
  for (std::size_t i = 0; i < span_const.rows(); ++i) {
    EXPECT_EQ(values_const[i], span_const[i]);
  }
}

TEST(SpanTest, TestMakeInitializerListSpan2d) {
  // Use invoke to ensure span lifetime is valid:
  std::invoke(
      [](auto span) {
        ASSERT_EQ(0, span(0, 0));
        ASSERT_EQ(1, span(0, 1));
        ASSERT_EQ(2, span(1, 0));
        ASSERT_EQ(3, span(1, 1));
      },
      make_array_span_2d<2, 2, ordering::row_major>({0, 1, 2, 3}));

  std::invoke(
      [](auto span) {
        ASSERT_EQ(0, span(0, 0));
        ASSERT_EQ(2, span(0, 1));
        ASSERT_EQ(1, span(1, 0));
        ASSERT_EQ(3, span(1, 1));
      },
      make_array_span_2d<2, 2, ordering::col_major>({0, 1, 2, 3}));

  // This should throw due to invalid size:
  auto construct_invalid_span = []() {
    make_array_span_2d<3, 4, ordering::row_major>({9.81, 3.14159});
  };
  ASSERT_THROW(construct_invalid_span(), std::runtime_error);
}

TEST(SpanTest, TestEigenColMajor) {
  const Eigen::Matrix<int, 2, 3> A = (Eigen::Matrix<int, 2, 3>() << 1, 2, 3, 4, 5, 6).finished();
  EXPECT_EQ(1, A.innerStride());
  EXPECT_EQ(2, A.outerStride());

  auto span = make_input_span<2, 3>(A);
  static_assert(1 == span.stride<0>(), "Should be computable at compile time");
  static_assert(2 == span.stride<1>(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(A, span);
}

TEST(SpanTest, TestEigenRowMajor) {
  const Eigen::Matrix<int, 4, 3, Eigen::RowMajor> B =
      (Eigen::Matrix<int, 4, 3>() << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).finished();
  EXPECT_EQ(1, B.innerStride());
  EXPECT_EQ(3, B.outerStride());

  auto span = make_input_span<4, 3>(B);
  static_assert(3 == span.stride<0>(), "Should be computable at compile time");
  static_assert(1 == span.stride<1>(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(B, span);
}

TEST(SpanTest, TestEigenColMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 5, 6>() << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                             13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
                                .finished();

  auto blk_1 = A.topRightCorner<2, 2>();
  auto span_1 = make_input_span<5, 6>(blk_1);
  ASSERT_EQ(1, span_1.stride<0>());
  ASSERT_EQ(5, span_1.stride<1>());
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  auto blk_2 = A.bottomRightCorner<3, 4>();
  auto span_2 = make_input_span<1, 5>(blk_2);
  ASSERT_EQ(1, span_2.stride<0>());
  ASSERT_EQ(5, span_2.stride<1>());
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);

  auto blk_3 = A.middleCols<3>(2).middleRows<3>(1);
  auto span_3 = make_input_span<3, 3>(blk_3);
  EXPECT_EIGEN_SPAN_EQ(blk_3, span_3);
}

TEST(SpanTest, TestEigenRowMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 3, 7, Eigen::RowMajor>() << 1, 2, 3, 4, 5, 6, 7, 8,
                             9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
                                .finished();

  auto blk_1 = A.block<3, 3>(0, 2);
  auto span_1 = make_input_span<3, 3>(blk_1);
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  auto blk_2 = A.bottomRightCorner<2, 3>();
  auto span_2 = make_input_span<2, 3>(blk_2);
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);
}

TEST(SpanTest, TestEigenMap) {
  const std::array<int, 9> data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  const Eigen::Map<const Eigen::Matrix3i, Eigen::Unaligned> map{data.data()};
  auto span = make_input_span<3, 3>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
}

// In the tests below, we fill elements that should be skipped by the strides w/ zero.
// Then we check that our span does not include any zero elements.
template <typename Dimensions, typename Strides>
void CheckNonZero(const math::span<const int, Dimensions, Strides>& span) {
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
  auto span = make_input_span<5, 4>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 5, 4, Eigen::RowMajor>, Eigen::Unaligned, DynamicStride>
      map_dynamic{data.data(), DynamicStride{8, 2}};
  auto span_dynamic = make_input_span<5, 4>(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

TEST(SpanTest, TestEigenMapOuterStride) {
  const std::array<int, 23> data = {1,  6, 11, 0, 0, 2,  7, 12, 0, 0,  3, 8,
                                    13, 0, 0,  4, 9, 14, 0, 0,  5, 10, 15};

  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, Eigen::OuterStride<5>> map{
      data.data()};
  auto span = make_input_span<3, 5>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{5, 1}};

  auto span_dynamic = make_input_span<3, 5>(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

TEST(SpanTest, TestEigenMapInnerAndOuterStride) {
  const std::array<int, 29> data = {1, 0, 4, 0, 7, 0, 0, 0, 0, 0, 0, 0, 2, 0, 5,
                                    0, 8, 0, 0, 0, 0, 0, 0, 0, 3, 0, 6, 0, 9};

  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, Eigen::Stride<12, 2>> map{
      data.data()};
  auto span = make_input_span<3, 4>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  EXPECT_EQ(1, span(0, 0));
  EXPECT_EQ(4, span(1, 0));
  EXPECT_EQ(2, span(0, 1));
  CheckNonZero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{12, 2}};

  auto span_dynamic = make_input_span<3, 3>(map_dynamic);
  static_assert(sizeof(span_dynamic) == 32, "");

  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  CheckNonZero(span_dynamic);
}

TEST(SpanTest, TestEigenNullMapAssertion) {
  // Constructing from null map should trigger our assertion macro.
  const Eigen::Map<const Eigen::Matrix<int, 3, 3>> map{nullptr};
  auto make_span = [&]() { make_input_span<3, 3>(map); };

  // TODO: Should we restore this assertion?
  //  ASSERT_THROW(make_span(), std::runtime_error);
}

namespace detail {
template <typename Derived, typename Scalar, typename Dimensions, typename Strides>
testing::AssertionResult ExpectEigenSpanEqual(const std::string& name_a, const std::string& name_b,
                                              const Eigen::MatrixBase<Derived>& a,
                                              const math::span<Scalar, Dimensions, Strides> b) {
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

}  // namespace math
