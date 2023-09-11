// Copyright 2023 Gareth Cross
#pragma once
#include "span.h"

// Define `MATH_SPAN_EIGEN_SUPPORT` prior to including this header to enable convenient conversion
// from Eigen types to spans. Once defined, you may call make_span_eigen() to construct span
// objects from Eigen matrices, maps, and blocks.
#ifdef MATH_SPAN_EIGEN_SUPPORT

#include <Eigen/Core>

namespace math {

namespace detail {
constexpr auto inherits_matrix_base_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_matrix_base_(const Eigen::MatrixBase<Derived>&) -> std::true_type;

// Get the row-stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_row_stride(const Eigen::MatrixBase<Derived>& mat) noexcept;

// Get the column stride of an eigen matrix.
template <typename Derived>
constexpr auto eigen_col_stride(const Eigen::MatrixBase<Derived>& mat) noexcept;

// Evaluates to std::true_type if `T` inherits form MatrixBase, otherwise std::false_type.
template <typename T>
using inherits_matrix_base = decltype(inherits_matrix_base_(std::declval<const T>()));

template <typename T>
using enable_if_inherits_matrix_base_t =
    typename std::enable_if<inherits_matrix_base<typename std::decay<T>::type>::value>::type;

}  // namespace detail

// Create a `Span` object from an eigen type. `mat` may be an Eigen::Matrix, Eigen::Block, or an
// Eigen::Map. The dimensionality of `mat` must be known at compile-time, but stride values may be
// runtime quantities.
//
// Note that if `T` is an Eigen::Map, it must be a non-null map or else:
//  - The user-defined `MATH_SPAN_RUNTIME_ASSERT` assertion macro will be invoked with a false
//    argument.
//  - Undefined behavior will occur when elements of the span are accessed.
template <typename T, typename = detail::enable_if_inherits_matrix_base_t<T>>
constexpr auto make_span_eigen(T&& mat) MATH_SPAN_MAYBE_NOEXCEPT {
  using TDecay = std::decay_t<T>;

  constexpr Eigen::Index RowsAtCompileTime = Eigen::DenseBase<TDecay>::RowsAtCompileTime;
  constexpr Eigen::Index ColsAtCompileTime = Eigen::DenseBase<TDecay>::ColsAtCompileTime;
  static_assert(RowsAtCompileTime != Eigen::Dynamic, "Rows must be known at compile time");
  static_assert(ColsAtCompileTime != Eigen::Dynamic, "Cols must be known at compile time");

  auto dims = make_constant_value_pack<static_cast<std::size_t>(RowsAtCompileTime),
                                       static_cast<std::size_t>(ColsAtCompileTime)>();
  auto strides = make_value_pack(detail::eigen_row_stride(mat), detail::eigen_col_stride(mat));
  return make_not_null_span(mat.data(), dims, strides);
}

namespace detail {

template <Eigen::Index InnerStride>
using eigen_stride_type_t =
    typename std::conditional<InnerStride == Eigen::Dynamic, dynamic,
                              constant<static_cast<std::size_t>(InnerStride)>>::type;

template <typename Derived>
constexpr auto eigen_row_stride(const Eigen::MatrixBase<Derived>& mat) noexcept {
  constexpr bool RowMajor = Eigen::MatrixBase<Derived>::IsRowMajor;
  using ReturnType =
      std::conditional_t<RowMajor,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>,
                         eigen_stride_type_t<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>>;
  return ReturnType{static_cast<std::size_t>(RowMajor ? mat.outerStride() : mat.innerStride())};
}

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
}  // namespace math

#endif  // MATH_SPAN_EIGEN_SUPPORT
