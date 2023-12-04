// Copyright 2023 Gareth Cross
#pragma once
#include "span.h"

// Define `MATH_SPAN_EIGEN_SUPPORT` prior to including this header to enable passing of eigen types
// to generated functions.
#ifdef MATH_SPAN_EIGEN_SUPPORT
#include <Eigen/Core>
#include <Eigen/Geometry>

namespace wf {

namespace detail {
constexpr auto inherits_matrix_base_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_matrix_base_(const Eigen::MatrixBase<Derived>&) -> std::true_type;

constexpr auto inherits_quaternion_base_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_quaternion_base_(const Eigen::QuaternionBase<Derived>&) -> std::true_type;

// Convert an eigen Stride to a `constant<>` or `dynamic`.
template <Eigen::Index Stride>
using eigen_stride_type_t =
    typename std::conditional<Stride == Eigen::Dynamic, dynamic,
                              constant<static_cast<std::size_t>(Stride)>>::type;

// Get the row-stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_row_stride(const Eigen::MatrixBase<Derived>& mat) noexcept {
  constexpr bool row_major = Eigen::MatrixBase<Derived>::IsRowMajor;
  using return_type =
      std::conditional_t<row_major,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>>;
  return return_type{static_cast<std::size_t>(row_major ? mat.outerStride() : mat.innerStride())};
}

// Get the column stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_col_stride(const Eigen::MatrixBase<Derived>& mat) noexcept {
  constexpr bool row_major = Eigen::MatrixBase<Derived>::IsRowMajor;
  using return_type =
      std::conditional_t<row_major,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>>;
  return return_type{static_cast<std::size_t>(row_major ? mat.innerStride() : mat.outerStride())};
}

}  // namespace detail

// Evaluates to std::true_type if `T` inherits from MatrixBase, otherwise std::false_type.
template <typename T>
using inherits_matrix_base = decltype(detail::inherits_matrix_base_(std::declval<const T>()));
template <typename T>
constexpr bool inherits_matrix_base_v = inherits_matrix_base<T>::value;

// Evaluates to `void` if `T` inherits from MatrixBase.
template <typename T>
using enable_if_inherits_matrix_base_t =
    typename std::enable_if<inherits_matrix_base_v<typename std::decay<T>::type>>::type;

// Evaluates to std::true_type if `T` inherits from QuaternionBase, otherwise std::false_type.
template <typename T>
using inherits_quaternion_base =
    decltype(detail::inherits_quaternion_base_(std::declval<const T>()));
template <typename T>
constexpr bool inherits_quaternion_base_v = inherits_quaternion_base<T>::value;

// Evaluates to `void` if `T` inherits from QuaternionBase.
template <typename T>
using enable_if_inherits_quaternion_base_t =
    typename std::enable_if<inherits_quaternion_base_v<typename std::decay<T>::type>>::type;

// Enable conversion of `MatrixBase` children to spans.
template <typename Dimensions, typename T>
struct convert_to_span<Dimensions, T, enable_if_inherits_matrix_base_t<T>> {
  template <typename U>
  constexpr auto convert(U&& mat) noexcept {
    using UDecay = typename std::decay<U>::type;

    constexpr Eigen::Index rows_at_compile_time = Eigen::DenseBase<UDecay>::RowsAtCompileTime;
    constexpr Eigen::Index cols_at_compile_time = Eigen::DenseBase<UDecay>::ColsAtCompileTime;
    static_assert(rows_at_compile_time != Eigen::Dynamic, "Rows must be known at compile time");
    static_assert(cols_at_compile_time != Eigen::Dynamic, "Cols must be known at compile time");

    constexpr auto dims =
        make_constant_value_pack<static_cast<std::size_t>(rows_at_compile_time),
                                 static_cast<std::size_t>(cols_at_compile_time)>();
    auto strides = make_value_pack(detail::eigen_row_stride(mat), detail::eigen_col_stride(mat));
    return make_span(mat.data(), dims, strides);
  }
};

// Enable conversion of `QuaternionBase` children to spans.
template <typename Dimensions, typename T>
struct convert_to_span<Dimensions, T, enable_if_inherits_quaternion_base_t<T>> {
  template <typename U>
  constexpr auto convert(U&& q) noexcept {
    // Quaternion is convertible to 4x1 column vector.
    constexpr auto dims = make_constant_value_pack<4, 1>();
    constexpr auto strides = make_constant_value_pack<1, 4>();
    return make_span(q.coeffs().data(), dims, strides);
  }
};

}  // namespace wf

#endif  // MATH_SPAN_EIGEN_SUPPORT
