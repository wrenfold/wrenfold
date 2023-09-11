// Copyright 2023 Gareth Cross
#pragma once
#include "span_detail.h"

// The user may optionally define `MATH_SPAN_RUNTIME_ASSERT` before importing this file. This macro
// is expected to take a boolean argument, and run a user-specified assert.
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

// Base class for multidimensional spans.
// Don't instantiate this directly, use `not_null_span` and `span` (defined below).
template <typename T, typename Dimensions, typename Strides>
class span_base {
 public:
  static_assert(detail::is_value_pack<Dimensions>::value,
                "Second template argument must be value_pack<...>");
  static_assert(detail::is_value_pack<Strides>::value,
                "Third template argument must be value_pack<...>");
  static_assert(Strides::length == Dimensions::length,
                "Number of strides must match number of dimensions");

  // Type alias members so this type smells something like a std::span or absl::span.
  using element_type = T;
  using value_type = std::remove_cv_t<T>;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  // Number of dimensions in this span. Must be at least one.
  static constexpr std::size_t num_dimensions = Dimensions::length;

  // Construct from non-null pointer and strides.
  constexpr span_base(T* data, Dimensions dims, Strides strides) noexcept
      : data_(data), dimensions_(dims), strides_(strides) {}

  // Number of rows. Valid for 1D and 2D spans.
  constexpr std::size_t rows() const noexcept { return dimensions_.template get<0>().value(); }

  // Number of columns. Valid for 2D spans.
  constexpr std::size_t cols() const noexcept { return dimensions_.template get<1>().value(); }

  // Access all dimensions.
  constexpr const Dimensions& dimensions() const noexcept { return dimensions_; }

  // Access all strides.
  constexpr const Strides& strides() const noexcept { return strides_; }

  // Access the stride for dimension `D`.
  template <std::size_t D>
  constexpr auto stride() const noexcept {
    static_assert(D < num_dimensions, "Dimension index is invalid");
    return strides_.template get<D>().value();
  }

  // Matrix-access operator.
  // Interpret (i, j) as row and column indices and return a reference.
  template <typename... Indices>
  constexpr reference operator()(Indices... indices) const {
    return data_[compute_index(indices...)];
  }

  // Array access operator, valid for 1D spans.
  constexpr reference operator[](std::ptrdiff_t index) const { return data_[compute_index(index)]; }

  // Compute linear index from (row, column) matrix indices.
  template <typename... Indices>
  constexpr std::ptrdiff_t compute_index(Indices... indices) const noexcept {
    static_assert(std::conjunction_v<std::is_convertible<Indices, std::ptrdiff_t>...>,
                  "Indices must be convertible to std::ptrdiff_t");
    static_assert(sizeof...(Indices) == num_dimensions,
                  "Number of indices much match number of dimensions");
    return compute_index_internal(std::make_integer_sequence<std::size_t, num_dimensions>(),
                                  indices...);
  }

  // Access pointer to data:
  constexpr pointer data() const noexcept { return data_; }

 private:
  template <typename I, typename... Is, std::size_t Dim, std::size_t... Dims>
  constexpr std::ptrdiff_t compute_index_internal(std::integer_sequence<std::size_t, Dim, Dims...>,
                                                  I i, Is... jk) const noexcept {
    return compute_index_internal<Dim, Dims...>(i, jk...);
  }

  template <std::size_t Dim, std::size_t... Dims, typename I, typename... Is>
  constexpr std::ptrdiff_t compute_index_internal(I i, Is... jk) const noexcept {
    return static_cast<std::ptrdiff_t>(stride<Dim>()) * static_cast<std::ptrdiff_t>(i) +
           compute_index_internal<Dims...>(jk...);
  }

  // Handle the final recursion.
  template <std::size_t Dim, typename I>
  constexpr std::ptrdiff_t compute_index_internal(I i) const noexcept {
    return stride<Dim>() * static_cast<std::ptrdiff_t>(i);
  }

  // TODO: Use the compressed pair trick on dimensions and strides?
  pointer data_;
  Dimensions dimensions_;
  Strides strides_;
};

// Forward declare.
template <typename T, typename Dimensions, typename Strides>
class span;

// A 2D span type used to pass matrix arguments to generated functions.
// `Rows` and `Cols` must be compile-time constants.
// `RowStride` and `ColStride` may be either Const<D> or dynamic.
template <typename T, typename Dimensions, typename Strides>
class not_null_span final : public span_base<T, Dimensions, Strides> {
 private:
  // Private unchecked version of the constructor. (`data` has already been validated)
  constexpr not_null_span(T* data, Dimensions dims, Strides strides, detail::unchecked) noexcept
      : Base(data, dims, strides) {}

 public:
  using Base = span_base<T, Dimensions, Strides>;

  // The equivalent nullable type.
  using equivalent_span = span<T, Dimensions, Strides>;

  // Construct from non-null pointer and strides.
  constexpr not_null_span(T* data, Dimensions dims, Strides strides) MATH_SPAN_MAYBE_NOEXCEPT
      : Base(data, dims, strides) {
    MATH_SPAN_RUNTIME_ASSERT(data != nullptr);
  }

  // Construct from non-null pointer and strides.
  // Specialization for when `U` is the non-const version of T.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr not_null_span(U* data, Dimensions dims, Strides strides) MATH_SPAN_MAYBE_NOEXCEPT
      : Base(const_cast<T*>(data), dims, strides) {
    MATH_SPAN_RUNTIME_ASSERT(data != nullptr);
  }

  // Cannot construct from nullptr.
  constexpr not_null_span(std::nullptr_t, Dimensions, Strides) noexcept = delete;
  constexpr not_null_span(std::nullptr_t) noexcept = delete;

  // Implicit construct if U is the non-const version of T.
  // Allows promotion from non-const to const span.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr not_null_span(not_null_span<U, Dimensions, Strides> span) noexcept
      : Base(span.data(), span.dimensions(), span.strides()) {}

  // Convert to a span that can be null.
  constexpr equivalent_span as_span() const noexcept {
    return {Base::data(), Base::dimensions(), Base::strides()};
  }

  // Create a const version of this span.
  constexpr not_null_span<const typename std::remove_const<T>::type, Dimensions, Strides> as_const()
      const noexcept {
    return {Base::data(), Base::dimensions(), Base::strides(), detail::unchecked{}};
  }

 private:
  template <typename U, typename D, typename S>
  friend class not_null_span;
};

// A 2D span used for optional output arguments. This version of the container is nullable.
template <typename T, typename Dimensions, typename Strides>
class span final : public span_base<T, Dimensions, Strides> {
 public:
  using Base = span_base<T, Dimensions, Strides>;

  // Construct from pointer (which may be null) and strides.
  constexpr span(T* data, Dimensions dims, Strides strides) noexcept : Base(data, dims, strides) {}

  // Construct from nullptr.
  constexpr span(std::nullptr_t) noexcept : Base(nullptr, Dimensions{}, Strides{}) {}

  // Implicit conversion to bool to check if this object is null.
  constexpr operator bool() const noexcept { return Base::data() != nullptr; }  // NOLINT

  // Convert to a not-null span.
  constexpr not_null_span<T, Dimensions, Strides> as_not_null_span() const
      MATH_SPAN_MAYBE_NOEXCEPT {
    return not_null_span<T, Dimensions, Strides>{Base::data(), Base::dimensions(), Base::strides()};
  }
};

// Shorthand to construct a non-null span from pointer, dimensions, and strides.
// `Dimensions` and `Strides` must be instances of `value_pack`.
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_not_null_span(T* data, Dimensions dims,
                                  Strides strides) MATH_SPAN_MAYBE_NOEXCEPT {
  using dimension_type = typename std::decay<Dimensions>::type;
  using stride_type = typename std::decay<Strides>::type;
  return not_null_span<T, dimension_type, stride_type>{data, dims, strides};
}

// Shorthand to construct a span from pointer, dimensions, and strides.
// `Dimensions` and `Strides` must be instances of `value_pack`.
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_span(T* data, Dimensions dims, Strides strides) noexcept {
  using dimension_type = typename std::decay<Dimensions>::type;
  using stride_type = typename std::decay<Strides>::type;
  return span<T, dimension_type, stride_type>{data, dims, strides};
}

// Create a span that is null.
template <typename T, std::size_t... Dims>
constexpr auto make_always_null_span() noexcept {
  return make_span<T>(nullptr, make_constant_value_pack<Dims...>(),
                      detail::make_zero_strides<sizeof...(Dims)>());
}

// Make a span from a 1D array.
template <typename T, typename = detail::enable_if_array_like_t<T>>
constexpr auto make_array_span(T&& array) noexcept {
  // TODO: In some cases (std::array for example) we could make the size static.
  const dynamic dim{array.size()};
  return make_span(array.data(), make_value_pack(dim), make_constant_value_pack<1>());
}

// Make a span from a 1D C-style array.
template <typename T, std::size_t N>
constexpr auto make_array_span(T (&array)[N]) noexcept {
  static_assert(N > 0, "Array must have at least one element.");
  return make_span(&array[0], make_constant_value_pack<N>(), make_constant_value_pack<1>());
}

}  // namespace math
