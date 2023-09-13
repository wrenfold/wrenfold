// Copyright 2023 Gareth Cross
#pragma once
#include "span.h"

// Define `MATH_SPAN_EIGEN_SUPPORT` prior to including this header to enable passing of eigen types
// to generated functions.
#ifdef MATH_SPAN_EIGEN_SUPPORT

#include <Eigen/Core>

namespace math {

namespace detail {
constexpr auto inherits_matrix_base_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_matrix_base_(const Eigen::MatrixBase<Derived>&) -> std::true_type;

// Evaluates to std::true_type if `T` inherits form MatrixBase, otherwise std::false_type.
template <typename T>
using inherits_matrix_base = decltype(inherits_matrix_base_(std::declval<const T>()));

template <typename T>
using enable_if_inherits_matrix_base_t =
    typename std::enable_if<inherits_matrix_base<typename std::decay<T>::type>::value>::type;

// Convert an eigen Stride to a `constant<>` or `dynamic`.
template <Eigen::Index Stride>
using eigen_stride_type_t =
    typename std::conditional<Stride == Eigen::Dynamic, dynamic,
                              constant<static_cast<std::size_t>(Stride)>>::type;

// Get the row-stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_row_stride(const Eigen::MatrixBase<Derived>& mat) noexcept {
  constexpr bool RowMajor = Eigen::MatrixBase<Derived>::IsRowMajor;
  using ReturnType =
      std::conditional_t<RowMajor,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>>;
  return ReturnType{static_cast<std::size_t>(RowMajor ? mat.outerStride() : mat.innerStride())};
}

// Get the column stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_col_stride(const Eigen::MatrixBase<Derived>& mat) noexcept {
  constexpr bool RowMajor = Eigen::MatrixBase<Derived>::IsRowMajor;
  using ReturnType =
      std::conditional_t<RowMajor,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>>;
  return ReturnType{static_cast<std::size_t>(RowMajor ? mat.innerStride() : mat.outerStride())};
}

}  // namespace detail

template <typename Dimensions, typename T>
struct convert_to_span<Dimensions, T, detail::enable_if_inherits_matrix_base_t<T>> {
  template <typename U>
  constexpr auto convert(U&& mat) noexcept {
    using UDecay = typename std::decay<U>::type;

    constexpr Eigen::Index RowsAtCompileTime = Eigen::DenseBase<UDecay>::RowsAtCompileTime;
    constexpr Eigen::Index ColsAtCompileTime = Eigen::DenseBase<UDecay>::ColsAtCompileTime;
    static_assert(RowsAtCompileTime != Eigen::Dynamic, "Rows must be known at compile time");
    static_assert(ColsAtCompileTime != Eigen::Dynamic, "Cols must be known at compile time");

    constexpr auto dims = make_constant_value_pack<static_cast<std::size_t>(RowsAtCompileTime),
                                                   static_cast<std::size_t>(ColsAtCompileTime)>();
    auto strides = make_value_pack(detail::eigen_row_stride(mat), detail::eigen_col_stride(mat));
    return make_span(mat.data(), dims, strides);
  }
};

}  // namespace math

#endif  // MATH_SPAN_EIGEN_SUPPORT
