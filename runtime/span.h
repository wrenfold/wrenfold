// Copyright 2023 Gareth Cross
#pragma once
#include <tuple>

// Define `MATH_SPAN_EIGEN_SUPPORT` prior to including this header to enable convenient conversion
// from Eigen types to `Span`. Once defined, you may call make_span_eigen() to construct `Span`
// objects from Eigen matrices, maps, and blocks.
#ifdef MATH_SPAN_EIGEN_SUPPORT
#include <Eigen/Core>
#endif  // MATH_SPAN_EIGEN_SUPPORT

//
#ifndef MATH_SPAN_RUNTIME_ASSERT
#define MATH_SPAN_RUNTIME_ASSERT(condition) \
  do {                                      \
    (void)sizeof((condition));              \
  } while (0)
#define MATH_SPAN_MAYBE_NOEXCEPT noexcept
#else
// If user did not specify MATH_SPAN_MAYBE_NOEXCEPT, define it as empty here:
#ifndef MATH_SPAN_MAYBE_NOEXCEPT
#define MATH_SPAN_MAYBE_NOEXCEPT
#endif  // ifndef MATH_SPAN_MAYBE_NOEXCEPT
#endif  // ifndef MATH_SPAN_RUNTIME_ASSERT

namespace math {

// Type used for strides computed at compile time.
template <std::size_t D>
class constant;

// Type used for strides computed at runtime.
class dynamic;

// Forward declare.
template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class span_base;

// Forward declare.
template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class span;

namespace detail {
// Enable if: `T` is the const version of `U`.
template <typename T, typename U>
using enable_if_adding_const_t =
    typename std::enable_if<std::is_const<T>::value &&
                            std::is_same<std::remove_const_t<T>, U>::value>::type;

// Enable if `T` is the same as `U` after const is removed from both.
template <typename T, typename U>
using enable_if_same_after_removing_const_t =
    typename std::enable_if<std::is_same<typename std::remove_const<T>::type,
                                         typename std::remove_const<T>::type>::value>::type;
}  // namespace detail

// A 2D span type used to pass matrix arguments to generated functions.
// `Rows` and `Cols` must be compile-time constants.
// `RowStride` and `ColStride` may be either Const<D> or dynamic.
// TODO: Extend this to `n` dimensions.
template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class not_null_span final : public span_base<T, Rows, Cols, RowStride, ColStride> {
 public:
  using Base = span_base<T, Rows, Cols, RowStride, ColStride>;

  // The equivalent nullable type.
  using equivalent_span = span<T, Rows, Cols, RowStride, ColStride>;

  // Construct from non-null pointer and strides.
  constexpr not_null_span(T* data, RowStride row_stride,
                          ColStride col_stride) MATH_SPAN_MAYBE_NOEXCEPT
      : Base(data, row_stride, col_stride) {
    MATH_SPAN_RUNTIME_ASSERT(data != nullptr);
  }

  // Construct from non-null pointer and strides.
  // Specialization for when `U` is the non-const version of T.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr not_null_span(U* data, RowStride row_stride,
                          ColStride col_stride) MATH_SPAN_MAYBE_NOEXCEPT
      : Base(const_cast<T*>(data), row_stride, col_stride) {
    MATH_SPAN_RUNTIME_ASSERT(data != nullptr);
  }

  // Cannot construct from nullptr.
  constexpr not_null_span(std::nullptr_t, RowStride, ColStride) noexcept = delete;
  constexpr not_null_span(std::nullptr_t) noexcept = delete;

  // Implicit construct if U is the non-const version of T.
  // Allows promotion from non-const to const span.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr not_null_span(not_null_span<U, Rows, Cols, RowStride, ColStride> span) noexcept
      : Base(span.data(), span.template stride<0>(), span.template stride<1>()) {}

  // Convert to an equivalent span w/ dynamic strides.
  constexpr not_null_span<T, Rows, Cols, dynamic, dynamic> with_dynamic_strides() const noexcept {
    return {Base::data(), dynamic(Base::template stride<0>().value()),
            dynamic(Base::template stride<1>().value())};
  }

  // Convert to a span that can be null.
  constexpr equivalent_span as_span() const noexcept {
    return {Base::data(), Base::template stride<0>(), Base::template stride<1>()};
  }
};

// A 2D span used for optional output arguments. This version of the container is nullable.
template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class span final : public span_base<T, Rows, Cols, RowStride, ColStride> {
 public:
  using Base = span_base<T, Rows, Cols, RowStride, ColStride>;

  // Construct from pointer (which may be null) and strides.
  constexpr span(T* data, RowStride row_stride, ColStride col_stride) noexcept
      : Base(data, row_stride, col_stride) {}

  // Construct from nullptr.
  constexpr span(std::nullptr_t) noexcept : Base(nullptr, RowStride{0}, ColStride{0}) {}

  // Implicit conversion to bool to check if this object is null.
  constexpr operator bool() const noexcept { return Base::data() != nullptr; }  // NOLINT

  // Convert to a not-null span.
  constexpr not_null_span<T, Rows, Cols, RowStride, ColStride> as_not_null_span() const
      MATH_SPAN_MAYBE_NOEXCEPT {
    return not_null_span<T, Rows, Cols, RowStride, ColStride>{
        Base::data(), Base::template stride<0>(), Base::template stride<1>()};
  }
};

// Create a null span.
template <typename T, std::size_t Rows, std::size_t Cols>
constexpr auto make_null_span() noexcept {
  return span<T, Rows, Cols, constant<0>, constant<0>>{nullptr};
}

// Represent a compile-time constant stride value.
template <std::size_t D>
class constant {
 public:
  constexpr constant() noexcept = default;
  explicit constexpr constant(std::size_t) noexcept {};

  static constexpr std::size_t value() noexcept { return D; }
};

// Represent a run-time stride value.
class dynamic {
 public:
  // Construct with stride value.
  explicit constexpr dynamic(std::size_t value) noexcept : value_(value) {}

  // Access the stride value.
  constexpr std::size_t value() const noexcept { return value_; }

 private:
  std::size_t value_;
};

// Equality operator for spans of the same type.
// Dimensions must match at compile time.
template <typename T, std::size_t Rows, std::size_t Cols, typename RS1, typename CS1, typename U,
          typename RS2, typename CS2,
          typename = detail::enable_if_same_after_removing_const_t<T, U>>
bool operator==(const not_null_span<T, Rows, Cols, RS1, CS1> a,
                const not_null_span<U, Rows, Cols, RS2, CS2> b) noexcept {
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
bool operator!=(const not_null_span<T, Rows, Cols, RS1, CS1> a,
                const not_null_span<T, Rows, Cols, RS2, CS2> b) noexcept {
  return !(a == b);
}

// Implementation of SpanBase for non-const row and col strides.
template <typename RowStride, typename ColStride>
class span_stride_storage {
 public:
  span_stride_storage(RowStride row_stride, ColStride col_stride)
      : strides_(row_stride, col_stride) {}

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
class span_stride_storage<constant<R>, constant<C>> {
 public:
  span_stride_storage(constant<R>, constant<C>) {}

  template <std::size_t D>
  constexpr auto stride() const noexcept {
    static_assert(D <= 1, "Only two dimensions are supported");
    using TupleType = std::tuple<constant<R>, constant<C>>;
    return std::tuple_element_t<D, TupleType>{};
  }
};

template <typename T, std::size_t Rows, std::size_t Cols, typename RowStride, typename ColStride>
class span_base : public span_stride_storage<RowStride, ColStride> {
 public:
  using Base = span_stride_storage<RowStride, ColStride>;

  // Type alias members so this type smells something like a std::span or absl::span.
  using element_type = T;
  using value_type = std::remove_cv_t<T>;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

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

 protected:
  // Construct from pointers and strides.
  constexpr span_base(T* data, RowStride row_stride, ColStride col_stride) noexcept
      : Base(row_stride, col_stride), data_{data} {}

 private:
  pointer data_;
};

#ifdef MATH_SPAN_EIGEN_SUPPORT

namespace detail {
constexpr auto inherits_matrix_base_(...) -> std::false_type;
template <typename Derived>
constexpr auto inherits_matrix_base_(const Eigen::MatrixBase<Derived>&) -> std::true_type;

// Get the row-stride of an eigen matrix.
template <typename Derived>
constexpr auto EigenRowStride(const Eigen::MatrixBase<Derived>& mat);

// Get the column stride of an eigen matrix.
template <typename Derived>
constexpr auto EigenColStride(const Eigen::MatrixBase<Derived>& mat);

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

  using return_type = not_null_span<ScalarType, static_cast<std::size_t>(RowsAtCompileTime),
                                    static_cast<std::size_t>(ColsAtCompileTime),
                                    decltype(row_stride), decltype(col_stride)>;
  return return_type{mat.data(), row_stride, col_stride};
}

namespace detail {

template <Eigen::Index InnerStride>
using EigenStrideType =
    typename std::conditional<InnerStride == Eigen::Dynamic, dynamic,
                              constant<static_cast<std::size_t>(InnerStride)>>::type;

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
