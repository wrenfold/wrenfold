// Copyright 2023 Gareth Cross
#include "span_test_assertions.h"

namespace wf {

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

TEST(SpanTest, TestMakeValuePack) {
  // Check that we get the right types.
  auto pack1 = make_value_pack(1, 3);
  static_assert(std::is_same_v<decltype(pack1), value_pack<dynamic, dynamic>>, "");
  // == to avoid ODR for static constexpr
  ASSERT_FALSE(true == decltype(pack1)::known_at_compile_time);
  ASSERT_EQ(2, static_cast<int>(decltype(pack1)::length));
  ASSERT_EQ(1, pack1.get<0>().value());
  ASSERT_EQ(3, pack1.get<1>().value());

  auto pack2 = make_value_pack(constant<4>{}, constant<6>{});
  static_assert(std::is_same_v<decltype(pack2), value_pack<constant<4>, constant<6>>>, "");
  ASSERT_TRUE(true == decltype(pack2)::known_at_compile_time);
  ASSERT_EQ(2, static_cast<int>(decltype(pack2)::length));
  ASSERT_EQ(4, pack2.get<0>().value());
  ASSERT_EQ(6, pack2.get<1>().value());

  auto pack3 = make_value_pack(10, 8, constant<3>{});
  static_assert(std::is_same_v<decltype(pack3), value_pack<dynamic, dynamic, constant<3>>>, "");
  ASSERT_FALSE(true == decltype(pack3)::known_at_compile_time);
  ASSERT_EQ(3, static_cast<int>(decltype(pack3)::length));
  ASSERT_EQ(10, pack3.get<0>().value());
  ASSERT_EQ(8, pack3.get<1>().value());
  ASSERT_EQ(3, pack3.get<2>().value());
}

TEST(SpanTest, TestStaticIndexing) {
  std::array<float, 9> unused_data{};

  const auto row_major = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                                   make_constant_value_pack<3, 1>());

  ASSERT_EQ(16, sizeof(row_major));
  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(1, row_major.compute_index(0, 1));
  ASSERT_EQ(3, row_major.compute_index(1, 0));
  ASSERT_EQ(5, row_major.compute_index(1, 2));

  const auto col_major = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                                   make_constant_value_pack<1, 4>());

  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(4, col_major.compute_index(0, 1));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(6, col_major.compute_index(2, 1));

  const auto both = make_span(unused_data.data(), make_constant_value_pack<3, 3>(),
                              make_constant_value_pack<3, 9>());

  ASSERT_EQ(0, both.compute_index(0, 0));
  ASSERT_EQ(9, both.compute_index(0, 1));
  ASSERT_EQ(3, both.compute_index(1, 0));
  ASSERT_EQ(15, both.compute_index(2, 1));
  ASSERT_EQ(24, both.compute_index(2, 2));
}

TEST(SpanTest, TestDynamicIndexing) {
  std::array<float, 12> unused_data{};

  const auto row_major = make_span(unused_data.data(), make_constant_value_pack<3, 4>(),
                                   make_value_pack(dynamic(8), constant<1>{}));

  ASSERT_EQ(0, row_major.compute_index(0, 0));
  ASSERT_EQ(8, row_major.compute_index(1, 0));
  ASSERT_EQ(11, row_major.compute_index(1, 3));
  ASSERT_EQ(17, row_major.compute_index(2, 1));

  const auto col_major = make_span(unused_data.data(), make_constant_value_pack<3, 4>(),
                                   make_value_pack(constant<1>{}, dynamic(6)));
  ASSERT_EQ(0, col_major.compute_index(0, 0));
  ASSERT_EQ(1, col_major.compute_index(1, 0));
  ASSERT_EQ(8, col_major.compute_index(2, 1));
}

TEST(SpanTest, TestConstructor) {
  std::array<int, 6> data = {0, 1, 2, 3, 4, 5};

  const auto span =
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
  static_assert(std::is_same_v<const int, decltype(const_span)::element_type>, "");
  EXPECT_EQ(span.data(), const_span.data());
  EXPECT_EQ(span, const_span);
}

TEST(SpanTest, TestSpanTraits) {
  auto c = make_constant_value_pack<0, 3, 7>();
  static_assert(is_value_pack_v<decltype(c)>, "");
  static_assert(is_constant_value_pack_v<decltype(c)>, "");
  static_assert(value_pack_length_v<decltype(c)> == 3, "");
  static_assert(constant_value_pack_axis_v<0, decltype(c)> == 0, "");
  static_assert(constant_value_pack_axis_v<1, decltype(c)> == 3, "");
  static_assert(constant_value_pack_axis_v<2, decltype(c)> == 7, "");

  auto d1 = make_value_pack(0, 1, 2);
  static_assert(is_value_pack_v<decltype(d1)>, "");
  static_assert(!is_constant_value_pack_v<decltype(d1)>, "");
  static_assert(value_pack_length_v<decltype(d1)> == 3, "");

  auto d2 = make_value_pack(constant<1>{}, 10, 12, constant<5>{});
  static_assert(is_value_pack_v<decltype(d2)>, "");
  static_assert(!is_constant_value_pack_v<decltype(d2)>, "");
  static_assert(value_pack_length_v<decltype(d2)> == 4, "");

  static_assert(!is_value_pack_v<std::string>, "");
  static_assert(!is_value_pack_v<int>, "");
}

TEST(SpanTest, TestMakeNullSpan) {
  constexpr auto span = make_always_null_span<2, 3>();
  EXPECT_FALSE(span);
  EXPECT_EQ(nullptr, span.data());
  EXPECT_EQ(2, span.rows());
  EXPECT_EQ(3, span.cols());
  EXPECT_EQ(0, span.stride<0>());
  EXPECT_EQ(0, span.stride<1>());
}

TEST(SpanTest, TestMakeSpanFromSpan) {
  // Spans can be passed to make_input_span and make_output_span
  std::array<int, 6> data = {0, 1, 2, 3, 4, 5};
  const auto span =
      make_span(data.data(), make_constant_value_pack<3, 2>(), make_constant_value_pack<1, 3>());

  const auto input_span = make_input_span<3, 2>(span);
  EXPECT_EQ(span.data(), input_span.data());
  EXPECT_EQ(3, input_span.rows());
  EXPECT_EQ(2, input_span.cols());
  EXPECT_EQ(1, input_span.stride<0>());
  EXPECT_EQ(3, input_span.stride<1>());

  const auto output_span = make_output_span<3, 2>(span);
  EXPECT_EQ(span.data(), output_span.data());
  EXPECT_EQ(3, output_span.rows());
  EXPECT_EQ(2, output_span.cols());
  EXPECT_EQ(1, output_span.stride<0>());
  EXPECT_EQ(3, output_span.stride<1>());
}

// A type we create, so we can specialized convert_to_span.
// These are used below in TestNoexceptPropagation.
struct non_throwing_type {};
struct throwing_type {};

template <typename Dimensions>
struct convert_to_span<Dimensions, non_throwing_type> {
  template <typename U>
  constexpr auto convert(U&&) noexcept {
    return make_always_null_span<Dimensions>();
  }
};

template <typename Dimensions>
struct convert_to_span<Dimensions, throwing_type> {
  template <typename U>
  constexpr auto convert(U&&) noexcept(false) {
    // If we don't explicitly throw something here, MSVC appears to relabel
    // the method as noexcept(true).
    throw std::runtime_error("Oh no!");
    return make_always_null_span<Dimensions>();
  }
};

TEST(SpanTest, TestNoexceptPropagation) {
  using dimensions = constant_value_pack<2, 3, 4>;
  static_assert(detail::is_convertible_to_span_v<dimensions, non_throwing_type>, "");
  static_assert(detail::is_convertible_to_span_v<dimensions, throwing_type>, "");

  static_assert(detail::is_nothrow_convertible_to_span_v<dimensions, non_throwing_type>, "");
  static_assert(!detail::is_nothrow_convertible_to_span_v<dimensions, throwing_type>, "");

  non_throwing_type no_throw{};
  static_assert(noexcept(make_input_span<2, 3, 4>(non_throwing_type{})), "");
  static_assert(noexcept(make_input_span<2, 3, 4>(no_throw)), "");
  static_assert(noexcept(make_output_span<2, 3, 4>(no_throw)), "");
  static_assert(noexcept(make_optional_output_span<2, 3, 4>(no_throw)), "");

  throwing_type yes_throw{};
  static_assert(noexcept(make_input_span<2, 3, 4>(yes_throw)) == false, "");
  static_assert(noexcept(make_output_span<2, 3, 4>(yes_throw)) == false, "");
  static_assert(noexcept(make_optional_output_span<2, 3, 4>(yes_throw)) == false, "");
}

TEST(SpanTest, TestMakeArraySpan) {
  std::array<int, 5> data = {1, 2, 3, 4, 5};
  const auto span = make_array_span(data);
  EXPECT_EQ(data.data(), span.data());
  EXPECT_EQ(5, span.rows());
  EXPECT_EQ(5, span.dimension<0>());
  EXPECT_EQ(1, span.stride<0>());
  for (int i = 0; i < data.size(); ++i) {
    EXPECT_EQ(i + 1, span[i]);
  }
}

TEST(SpanTest, TestMakeEmptyVectorSpan) {
  const std::vector<int> data;
  const auto span = make_array_span(static_cast<const std::vector<int>&>(data));
  EXPECT_EQ(nullptr, span.data());
  EXPECT_EQ(0, span.rows());
  EXPECT_EQ(0, span.dimension<0>());
  EXPECT_EQ(1, span.stride<0>());
}

TEST(SpanTest, TestMakeVectorSpan) {
  std::vector<int> data = {5, 3, 8, 22};
  const auto span = make_array_span(data);
  EXPECT_EQ(data.data(), span.data());
  EXPECT_EQ(data.size(), span.rows());
  EXPECT_EQ(1, span.stride<0>());
  for (std::size_t i = 0; i < data.size(); ++i) {
    EXPECT_EQ(data[i], span[i]);
  }
}

TEST(SpanTest, TestMakeCArraySpan) {
  int values[6] = {1, 1, 2, 3, 5, 8};
  const auto span = make_array_span(values);

  static_assert(6 == span.rows(), "Size must be known at compile time");
  EXPECT_EQ(&values[0], span.data());
  for (std::size_t i = 0; i < span.rows(); ++i) {
    EXPECT_EQ(values[i], span[i]);
  }

  // Access a subsection of the array via a 1D block:
  const auto sub_span = span.block(make_constant_value_pack<1>(), make_value_pack(2));
  EXPECT_EQ(2, sub_span.rows());
  EXPECT_EQ(values[1], sub_span[0]);
  EXPECT_EQ(values[2], sub_span[1]);

  // Construction from const array:
  const int values_const[4] = {13, 21, 34, 55};
  const auto span_const = make_array_span(values_const);
  static_assert(4 == span_const.rows(), "Size must be known at compile time");
  EXPECT_EQ(&values_const[0], span_const.data());
  for (std::size_t i = 0; i < span_const.rows(); ++i) {
    EXPECT_EQ(values_const[i], span_const[i]);
  }
}

TEST(SpanTest, TestMake3DSpan) {
  struct Xyz {
    int x{0};
    int y{0};
    int z{0};
  };

  constexpr int num_rows = 4;
  constexpr int num_cols = 3;
  constexpr int num_channels = 2;

  std::array<Xyz, static_cast<std::size_t>(num_rows * num_cols * num_channels)> data{};
  std::size_t index = 0;
  for (int x = 0; x < num_rows; ++x) {
    for (int y = 0; y < num_cols; ++y) {
      for (int z = 0; z < num_channels; ++z, ++index) {
        data[index] = {x, y, z};
      }
    }
  }

  // Make a 3-dimensional span:
  const auto span =
      make_span(&data[0], make_constant_value_pack<num_rows, num_cols, num_channels>(),
                make_constant_value_pack<num_cols * num_channels, num_channels, 1>());
  EXPECT_EQ(num_rows, span.dimension<0>());
  EXPECT_EQ(num_cols, span.dimension<1>());
  EXPECT_EQ(num_channels, span.dimension<2>());

  for (int x = 0; x < num_rows; ++x) {
    for (int y = 0; y < num_cols; ++y) {
      for (int z = 0; z < num_channels; ++z) {
        EXPECT_EQ(x, span(x, y, z).x);
        EXPECT_EQ(y, span(x, y, z).y);
        EXPECT_EQ(z, span(x, y, z).z);
      }
    }
  }
}

TEST(SpanTest, TestEigenColMajor) {
  // clang-format off
  const Eigen::Matrix<int, 2, 3> A = (Eigen::Matrix<int, 2, 3>() <<
      1, 2, 3,
      4, 5, 6).finished();
  // clang-format on
  EXPECT_EQ(1, A.innerStride());
  EXPECT_EQ(2, A.outerStride());

  const auto span = make_input_span<2, 3>(A);
  static_assert(1 == span.stride<0>(), "Should be computable at compile time");
  static_assert(2 == span.stride<1>(), "Should be computable at compile time");
  EXPECT_EQ(2, span.dimension<0>());
  EXPECT_EQ(3, span.dimension<1>());
  EXPECT_EIGEN_SPAN_EQ(A, span);

  // get a sub-block
  const auto span_blk_1 =
      span.block(make_constant_value_pack<0, 0>(), make_constant_value_pack<2, 2>());
  static_assert(1 == span_blk_1.stride<0>(), "Should be computable at compile time");
  static_assert(2 == span_blk_1.stride<1>(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(A.leftCols<2>(), span_blk_1);

  const auto span_blk_2 = span.block(make_value_pack(1, 1), make_constant_value_pack<1, 2>());
  EXPECT_EIGEN_SPAN_EQ(A.bottomRightCorner(1, 2), span_blk_2);

  // get a sub-block from a sub-block
  const auto span_blk_3 =
      span.block(make_value_pack(0, 1), make_constant_value_pack<2, 2>())
          .block(make_constant_value_pack<0, 1>(), make_constant_value_pack<2, 1>());
  EXPECT_EIGEN_SPAN_EQ(A.rightCols<1>(), span_blk_3);
}

TEST(SpanTest, TestEigenRowMajor) {
  // clang-format off
  const Eigen::Matrix<int, 4, 3, Eigen::RowMajor> B =
      (Eigen::Matrix<int, 4, 3>() <<
          1, 2, 3,
          4, 5, 6,
          7, 8, 9,
          10, 11, 12).finished();
  // clang-format on
  EXPECT_EQ(1, B.innerStride());
  EXPECT_EQ(3, B.outerStride());

  const auto span = make_input_span<4, 3>(B);
  static_assert(3 == span.stride<0>(), "Should be computable at compile time");
  static_assert(1 == span.stride<1>(), "Should be computable at compile time");
  EXPECT_EIGEN_SPAN_EQ(B, span);

  const auto span_blk = span.block(make_value_pack(1, 1), make_value_pack(2, 1));
  EXPECT_EIGEN_SPAN_EQ(B.block(1, 1, 2, 1), span_blk);
}

TEST(SpanTest, TestEigenColMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 5, 6>() << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                             13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
                                .finished();
  auto A_span = make_input_span<5, 6>(A.block<5, 6>(0, 0));

  auto blk_1 = A.topRightCorner<2, 2>();
  auto span_1 = make_input_span<2, 2>(blk_1);
  ASSERT_EQ(1, span_1.stride<0>());
  ASSERT_EQ(5, span_1.stride<1>());
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  auto blk_2 = A.bottomRightCorner<3, 4>();
  auto span_2 = make_input_span<3, 4>(blk_2);
  ASSERT_EQ(1, span_2.stride<0>());
  ASSERT_EQ(5, span_2.stride<1>());
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);

  // compare to pulling out block with the span itself
  EXPECT_EIGEN_SPAN_EQ(
      blk_2, A_span.block(make_constant_value_pack<2, 2>(), make_constant_value_pack<3, 4>()));
  EXPECT_EIGEN_SPAN_EQ(blk_2,
                       A_span.block(make_constant_value_pack<2, 2>(), make_value_pack(3, 4)));
  EXPECT_EIGEN_SPAN_EQ(blk_2,
                       A_span.block(make_value_pack(2, 2), make_constant_value_pack<3, 4>()));

  auto blk_3 = A.middleCols<3>(2).middleRows<3>(1);
  auto span_3 = make_input_span<3, 3>(blk_3);
  EXPECT_EIGEN_SPAN_EQ(blk_3, span_3);
}

TEST(SpanTest, TestEigenRowMajorBlock) {
  const Eigen::MatrixXi A = (Eigen::Matrix<int, 3, 7, Eigen::RowMajor>() << 1, 2, 3, 4, 5, 6, 7, 8,
                             9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
                                .finished();

  const auto blk_1 = A.block<3, 3>(0, 2);
  const auto span_1 = make_input_span<3, 3>(blk_1);
  EXPECT_EIGEN_SPAN_EQ(blk_1, span_1);

  const auto blk_2 = A.bottomRightCorner<2, 3>();
  const auto span_2 = make_input_span<2, 3>(blk_2);
  EXPECT_EIGEN_SPAN_EQ(blk_2, span_2);
}

TEST(SpanTest, TestEigenMap) {
  const std::array<int, 9> data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  const Eigen::Map<const Eigen::Matrix3i, Eigen::Unaligned> map{data.data()};
  const auto span = make_input_span<3, 3>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
}

// In the tests below, we fill elements that should be skipped by the strides w/ zero.
// Then we check that our span does not include any zero elements.
template <typename Dimensions, typename Strides>
void check_non_zero(const wf::span<const int, Dimensions, Strides>& span) {
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
  const auto span = make_input_span<5, 4>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  check_non_zero(span);

  const auto sub_span_1 = span.block(make_value_pack(2, 2), make_constant_value_pack<3, 2>());
  EXPECT_EIGEN_SPAN_EQ(map.block(2, 2, 3, 2), sub_span_1);
  check_non_zero(sub_span_1);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 5, 4, Eigen::RowMajor>, Eigen::Unaligned, DynamicStride>
      map_dynamic{data.data(), DynamicStride{8, 2}};
  const auto span_dynamic = make_input_span<5, 4>(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  check_non_zero(span_dynamic);

  const auto sub_span_2 =
      span_dynamic.block(make_value_pack(1, 2), make_constant_value_pack<3, 2>());
  EXPECT_EIGEN_SPAN_EQ(map.block(1, 2, 3, 2), sub_span_2);
  check_non_zero(sub_span_2);
}

TEST(SpanTest, TestEigenMapOuterStride) {
  const std::array<int, 23> data = {1,  6, 11, 0, 0, 2,  7, 12, 0, 0,  3, 8,
                                    13, 0, 0,  4, 9, 14, 0, 0,  5, 10, 15};

  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, Eigen::OuterStride<5>> map{
      data.data()};
  const auto span = make_input_span<3, 5>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  check_non_zero(span);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 5>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{5, 1}};

  const auto span_dynamic = make_input_span<3, 5>(map_dynamic);
  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  check_non_zero(span_dynamic);
}

TEST(SpanTest, TestEigenMapInnerAndOuterStride) {
  const std::array<int, 29> data = {1, 0, 4, 0, 7, 0, 0, 0, 0, 0, 0, 0, 2, 0, 5,
                                    0, 8, 0, 0, 0, 0, 0, 0, 0, 3, 0, 6, 0, 9};

  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, Eigen::Stride<12, 2>> map{
      data.data()};
  const auto span = make_input_span<3, 3>(map);
  EXPECT_EIGEN_SPAN_EQ(map, span);
  EXPECT_EQ(1, span(0, 0));
  EXPECT_EQ(4, span(1, 0));
  EXPECT_EQ(2, span(0, 1));
  check_non_zero(span);

  const auto sub_span_1 = span.block(make_value_pack(constant<0>{}, dynamic(1)),
                                     make_value_pack(dynamic(2), constant<2>{}));
  EXPECT_EIGEN_SPAN_EQ(map.block(0, 1, 2, 2), sub_span_1);
  check_non_zero(sub_span_1);

  using DynamicStride = Eigen::Stride<Eigen::Dynamic, Eigen::Dynamic>;
  const Eigen::Map<const Eigen::Matrix<int, 3, 3>, Eigen::Unaligned, DynamicStride> map_dynamic{
      data.data(), DynamicStride{12, 2}};

  const auto span_dynamic = make_input_span<3, 3>(map_dynamic);
  static_assert(sizeof(span_dynamic) == 32, "");

  EXPECT_EIGEN_SPAN_EQ(map_dynamic, span_dynamic);
  check_non_zero(span_dynamic);
}

TEST(SpanTest, TestEigenQuaternion) {
  Eigen::Quaternion<double> q{1.0, -0.5, 0.23, 0.11};

  const auto q_input = make_input_span<4, 1>(q);
  EXPECT_EQ(4, q_input.dimension<0>());
  EXPECT_EQ(1, q_input.dimension<1>());
  EXPECT_EQ(1, q_input.stride<0>());
  EXPECT_EQ(4, q_input.stride<1>());
  EXPECT_EIGEN_SPAN_EQ(q.coeffs(), q_input);

  const auto q_output = make_output_span<4, 1>(q);
  EXPECT_EIGEN_SPAN_EQ(q.coeffs(), q_output);
}

}  // namespace wf
