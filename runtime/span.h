// Copyright 2023 Gareth Cross
#pragma once
#include <tuple>

// Define `MATH_SPAN_EIGEN_SUPPORT` prior to including this header to enable convenient conversion
// from Eigen types to `Span`. Once defined, you may call make_span_eigen() to construct `Span`
// objects from Eigen matrices, maps, and blocks.
#ifdef MATH_SPAN_EIGEN_SUPPORT
#include <Eigen/Core>
#endif  // MATH_SPAN_EIGEN_SUPPORT

namespace math {

// Type used for strides computed at compile time.
template <std::size_t D>
class Const;

// Type used for strides computed at runtime.
class Dynamic;

// Forward declaration.
template <typename RowStride, typename ColStride>
class SpanBase;

// A 2D span type used to pass matrix arguments to generated functions.
// `Rows` and `Cols` must be compile-time constants.
// `RowStride` and `ColStride` may be either Const<D> or Dynamic.
// TODO: Extend this to `n` dimensions.
template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class Span final : public SpanBase<RowStride, ColStride> {
 public:
  using Base = SpanBase<RowStride, ColStride>;

  template <typename U>
  using EnableIfAddingConst =
      std::enable_if_t<std::is_const_v<T> && std::is_same_v<std::remove_const_t<T>, U>>;

  // Type alias members so this type smells something like a std::span or absl::span.
  using element_type = T;
  using value_type = std::remove_cv_t<T>;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  // Construct from pointers and strides.
  constexpr Span(T* data, RowStride row_stride, ColStride col_stride) noexcept
      : Base(row_stride, col_stride), data_{data} {}

  // Implicit construct if U is the non-const version of T.
  // Allows promotion from non-const to const span.
  template <typename U, typename = EnableIfAddingConst<U>>
  constexpr Span(Span<U, Rows, Cols, RowStride, ColStride> span) noexcept
      : Base(span.template stride<0>(), span.template stride<1>()), data_{span.data_} {}

  // Number of rows.
  constexpr auto rows() const noexcept { return Rows; }

  // Number of columns.
  constexpr auto cols() const noexcept { return Cols; }

  // Matrix-access operator.
  // Interpret (i, j) as row and column indices and return a reference.
  constexpr reference operator()(std::size_t i, std::size_t j) const {
    return data_[compute_index(i, j)];
  }

  // Compute linear index from (row, column) matrix indices.
  constexpr std::size_t compute_index(std::size_t i, std::size_t j) const noexcept {
    return Base::template stride<0>().value() * i + Base::template stride<1>().value() * j;
  }

  // Access pointer to data:
  constexpr pointer data() const noexcept { return data_; }

  // Convert to an equivalent span w/ dynamic strides.
  constexpr Span<T, Rows, Cols, Dynamic, Dynamic> with_dynamic_strides() const noexcept {
    return {data_, Dynamic(Base::template stride<0>().value()),
            Dynamic(Base::template stride<1>().value())};
  }

  // Implicit cast to bool to check if the span is null.
  constexpr operator bool() const noexcept { return data_ != nullptr; }

 private:
  pointer data_;
};

// Represent a compile-time constant stride value.
template <std::size_t D>
class Const {
 public:
  constexpr Const() noexcept = default;
  explicit constexpr Const(std::size_t) noexcept {};

  static constexpr std::size_t value() noexcept { return D; }
};

// Represent a run-time stride value.
class Dynamic {
 public:
  // Construct with stride value.
  explicit constexpr Dynamic(std::size_t value) noexcept : value_(value) {}

  // Access the stride value.
  constexpr std::size_t value() const noexcept { return value_; }

 private:
  std::size_t value_;
};

// Equality operator for spans of the same type.
// Dimensions must match at compile time.
template <typename T, std::size_t Rows, std::size_t Cols, typename RS1, typename CS1, typename RS2,
          typename CS2>
bool operator==(const Span<T, Rows, Cols, RS1, CS1> a,
                const Span<T, Rows, Cols, RS2, CS2> b) noexcept {
  if (!a || !b) {
    return !a && !b;
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

// Non-equality operator for spans of the same type.
// Dimensions must match at compile time.
template <typename T, std::size_t Rows, std::size_t Cols, typename RS1, typename CS1, typename RS2,
          typename CS2>
bool operator!=(const Span<T, Rows, Cols, RS1, CS1> a,
                const Span<T, Rows, Cols, RS2, CS2> b) noexcept {
  return !(a == b);
}

// Implementation of SpanBase for non-const row and col strides.
template <typename RowStride, typename ColStride>
class SpanBase {
 public:
  SpanBase(RowStride row_stride, ColStride col_stride) : strides_(row_stride, col_stride) {}

  // Get the stride for the D'th dimension.
  template <std::size_t D>
  constexpr auto stride() const noexcept {
    static_assert(D <= 1, "Only two dimensions are supported");
    return std::get<D>(strides_);
  }

 private:
  // TODO: Use uint32_t for these to minimize object size?
  std::tuple<RowStride, ColStride> strides_;
};

// Specialization of SpanBase for constant Row and Col strides.
// Removes any storage for the strides to minimize object size.
template <std::size_t R, std::size_t C>
class SpanBase<Const<R>, Const<C>> {
 public:
  SpanBase(Const<R>, Const<C>) {}

  // Get the stride for the D'th dimension.
  template <std::size_t D>
  constexpr auto stride() const noexcept {
    static_assert(D <= 1, "Only two dimensions are supported");
    return std::get<D>(std::make_pair(Const<R>{}, Const<C>{}));
  }
};

#ifdef MATH_SPAN_EIGEN_SUPPORT

namespace detail {
constexpr auto InheritsMatrixBase_(...) -> std::false_type;
template <typename Derived>
constexpr auto InheritsMatrixBase_(const Eigen::MatrixBase<Derived>&) -> std::true_type;

// Get the row-stride of an eigen matrix.
template <typename Derived>
constexpr auto EigenRowStride(const Eigen::MatrixBase<Derived>& mat);

// Get the column stride of an eigen matrix.
template <typename Derived>
constexpr auto EigenColStride(const Eigen::MatrixBase<Derived>& mat);

// Evaluates to std::true_type if `T` inherits form MatrixBase, otherwise std::false_type.
template <typename T>
using InheritsMatrixBase = decltype(InheritsMatrixBase_(std::declval<T>()));
}  // namespace detail

// Create a `Span` object from an eigen type. `mat` may be an Eigen::Matrix, Eigen::Block, or an
// Eigen::Map. The dimensionality of `mat` must be known at compile-time, but stride values may be
// runtime quantities.
template <typename T, typename = std::enable_if_t<detail::InheritsMatrixBase<T>::value>>
constexpr auto make_span_eigen(T&& mat) noexcept {
  using TDecay = std::decay_t<T>;
  using ScalarType =
      std::conditional_t<std::is_const_v<std::remove_pointer_t<decltype(mat.data())>>,
                         const typename Eigen::MatrixBase<TDecay>::Scalar,
                         typename Eigen::MatrixBase<TDecay>::Scalar>;

  constexpr Eigen::Index RowsAtCompileTime = Eigen::DenseBase<TDecay>::RowsAtCompileTime;
  constexpr Eigen::Index ColsAtCompileTime = Eigen::DenseBase<TDecay>::ColsAtCompileTime;
  static_assert(RowsAtCompileTime != Eigen::Dynamic, "Rows must be known at compile time");
  static_assert(ColsAtCompileTime != Eigen::Dynamic, "Cols must be known at compile time");

  auto row_stride = detail::EigenRowStride(mat);
  auto col_stride = detail::EigenColStride(mat);
  return Span<ScalarType, static_cast<std::size_t>(RowsAtCompileTime),
              static_cast<std::size_t>(ColsAtCompileTime), decltype(row_stride),
              decltype(col_stride)>(mat.data(), row_stride, col_stride);
}

namespace detail {

template <Eigen::Index InnerStride>
using EigenStrideType =
    typename std::conditional<InnerStride == Eigen::Dynamic, Dynamic,
                              Const<static_cast<std::size_t>(InnerStride)>>::type;

template <typename Derived>
constexpr auto EigenRowStride(const Eigen::MatrixBase<Derived>& mat) {
  constexpr bool RowMajor = Eigen::MatrixBase<Derived>::IsRowMajor;
  using ReturnType =
      std::conditional_t<RowMajor,
                         EigenStrideType<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>,
                         EigenStrideType<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>>;
  return ReturnType{static_cast<std::size_t>(RowMajor ? mat.outerStride() : mat.innerStride())};
}

template <typename Derived>
constexpr auto EigenColStride(const Eigen::MatrixBase<Derived>& mat) {
  constexpr bool RowMajor = Eigen::MatrixBase<Derived>::IsRowMajor;
  using ReturnType =
      std::conditional_t<RowMajor,
                         EigenStrideType<Eigen::MatrixBase<Derived>::InnerStrideAtCompileTime>,
                         EigenStrideType<Eigen::MatrixBase<Derived>::OuterStrideAtCompileTime>>;
  return ReturnType{static_cast<std::size_t>(RowMajor ? mat.innerStride() : mat.outerStride())};
}

}  // namespace detail

#endif  //  MATH_SPAN_EIGEN_SUPPORT

}  // namespace math
