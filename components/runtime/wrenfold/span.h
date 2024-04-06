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
#endif  // ifndef MATH_SPAN_RUNTIME_ASSERT

namespace wf {

// Type used for strides computed at compile time.
template <std::size_t D>
class constant;

// Type used for strides computed at runtime.
class dynamic;

// Fwd declare.
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_span(T* data, Dimensions dims, Strides strides) noexcept;

// This is the trait you implement to add support for your custom argument type.
//
// An example implementation for a simple 2D matrix type with compile-time dimensions might look
// something like:
//
//  template <typename Dimensions, int Rows, int Cols>
//  struct convert_to_span<Dimensions, MyMatrixType<Rows, Cols>> {
//     // Forwarding reference to accept both const and non-const.
//     template <typename U>
//     constexpr auto convert(U&& matrix) noexcept {
//        static_assert(constant_value_pack_axis_v<0, Dimensions> == Rows);
//        static_assert(constant_value_pack_axis_v<1, Dimensions> == Cols);
//        // Assuming row-major storage:
//        constexpr auto strides = make_constant_value_pack<Cols, 1>();
//        return make_span(matrix.data(), make_constant_value_pack<Rows, Cols>(), strides);
//     }
//  };
//
// See `span_eigen.h` for an example implementation.
template <typename Dimensions, typename T, typename = void>
struct convert_to_span;

// Base class for multidimensional spans.
// Don't instantiate this directly, use `make_span` or specialize the `convert_to_span` struct.
template <typename T, typename Dimensions, typename Strides>
class span {
 public:
  static_assert(is_value_pack<Dimensions>::value,
                "Second template argument must be value_pack<...>");
  static_assert(is_value_pack<Strides>::value, "Third template argument must be value_pack<...>");
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

  // Construct from pointer and strides.
  constexpr span(T* data, Dimensions dims, Strides strides) noexcept
      : data_(data), dimensions_(dims), strides_(strides) {}

  // Construct from  pointer and strides.
  // Specialization for when `U` is the non-const version of T.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr span(U* data, Dimensions dims, Strides strides) noexcept
      : span(const_cast<T*>(data), dims, strides) {}

  // Implicit construct if U is the non-const version of T.
  // Allows promotion from non-const to const span.
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr span(const span<U, Dimensions, Strides>& s) noexcept
      : span(const_cast<T*>(s.data()), s.dimensions(), s.strides()) {}

  // Number of rows. Valid for 1D and 2D spans.
  constexpr std::size_t rows() const noexcept { return dimensions_.template get<0>().value(); }

  // Number of columns. Valid for 2D spans.
  constexpr std::size_t cols() const noexcept { return dimensions_.template get<1>().value(); }

  // Access all dimensions.
  constexpr const Dimensions& dimensions() const noexcept { return dimensions_; }

  // Access all strides.
  constexpr const Strides& strides() const noexcept { return strides_; }

  // Access the dimension for axis `D`.
  template <std::size_t D>
  constexpr auto dimension() const noexcept {
    static_assert(D < num_dimensions, "dimension index is invalid");
    return dimensions_.template get<D>().value();
  }

  // Access the stride for axis `D`.
  template <std::size_t D>
  constexpr auto stride() const noexcept {
    static_assert(D < num_dimensions, "dimension index is invalid");
    return strides_.template get<D>().value();
  }

  // Matrix-access operator.
  // Interpret (i, j) as row and column indices and return a reference.
  template <typename... Indices>
  constexpr reference operator()(Indices... indices) const noexcept {
    return data_[compute_index(indices...)];
  }

  // Array access operator, valid for 1D spans.
  constexpr reference operator[](std::ptrdiff_t index) const noexcept {
    return data_[compute_index(index)];
  }

  // Compute linear index from (row, column) matrix indices.
  template <typename... Indices>
  constexpr std::ptrdiff_t compute_index(Indices... indices) const noexcept {
    static_assert(detail::conjunction<std::is_convertible<Indices, std::ptrdiff_t>...>::value,
                  "Indices must be convertible to std::ptrdiff_t");
    static_assert(sizeof...(Indices) == num_dimensions,
                  "Number of indices much match number of dimensions");
    return compute_index_internal(std::make_integer_sequence<std::size_t, num_dimensions>(),
                                  indices...);
  }

  // Access pointer to data:
  constexpr pointer data() const noexcept { return data_; }

  // Implicit conversion to bool to check if this object is null.
  constexpr operator bool() const noexcept { return data_ != nullptr; }  // NOLINT

  // Create a const version of this span.
  constexpr span<const typename std::remove_const<T>::type, Dimensions, Strides> as_const()
      const noexcept {
    return {data_, dimensions(), strides()};
  }

  // Access a sub-block of the span with the given offsets and dimensions.
  template <typename O, typename D>
  constexpr auto block(O offsets, D dims) const noexcept {
    static_assert(num_dimensions == O::length, "Incorrect # of dimensions in offsets.");
    static_assert(num_dimensions == D::length, "Incorrect # of dimensions in sizes.");
    auto start_index = compute_index_internal_vp(offsets);
    return make_span(data() + start_index, dims, strides());
  }

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

  template <typename... Values, std::size_t... Dims>
  constexpr std::ptrdiff_t compute_index_internal_vp(
      std::integer_sequence<std::size_t, Dims...>,
      const value_pack<Values...>& values) const noexcept {
    static_assert(sizeof...(Dims) == sizeof...(Values), "");
    return compute_index_internal(std::make_integer_sequence<std::size_t, sizeof...(Values)>(),
                                  values.template get<Dims>().value()...);
  }

  // Compute index from a value pack of offsets.
  template <typename... Values>
  constexpr std::ptrdiff_t compute_index_internal_vp(
      const value_pack<Values...>& values) const noexcept {
    return compute_index_internal_vp(std::make_integer_sequence<std::size_t, sizeof...(Values)>(),
                                     values);
  }

  // TODO: Use the compressed pair trick on dimensions and strides?
  pointer data_;
  Dimensions dimensions_;
  Strides strides_;
};

// Shorthand to construct a span from pointer, dimensions, and strides.
// `Dimensions` and `Strides` must be instances of `value_pack`.
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_span(T* data, Dimensions dims, Strides strides) noexcept {
  using dimension_type = typename std::decay<Dimensions>::type;
  using stride_type = typename std::decay<Strides>::type;
  return span<T, dimension_type, stride_type>{data, dims, strides};
}

// Create a span that is null with the specified type and dimensions.
// Accepts a variadic list of compile-time integers for dimensions.
template <std::size_t... Dims>
constexpr auto make_always_null_span() noexcept {
  return make_span<detail::void_type>(nullptr, make_constant_value_pack<Dims...>(),
                                      detail::make_zero_value_pack<sizeof...(Dims)>());
}

// Create a span that is null with the specified type and dimensions.
// `Dimensions` is a value_pack.
template <typename Dimensions>
constexpr auto make_always_null_span() noexcept {
  static_assert(is_value_pack<Dimensions>::value, "Dimensions must be a value_pack");
  return make_span<detail::void_type>(nullptr, Dimensions::zero_initialized(),
                                      detail::make_zero_value_pack<Dimensions::length>());
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

namespace detail {

// Remove reference and const modifiers.
template <typename T>
using remove_const_and_ref_t =
    typename std::remove_const<typename std::remove_reference<T>::type>::type;

// Evaluate to specialized `convert_to_span` for T (minus const and reference attributes).
template <typename Dimensions, typename T>
using span_converter = convert_to_span<Dimensions, detail::remove_const_and_ref_t<T>>;

// Evaluates to true_type if <Dimensions, T> is a valid specialization of `convert_to_span`.
template <typename Dimensions, typename T, typename = void>
struct is_convertible_to_span : std::false_type {};
template <typename Dimensions, typename T>
struct is_convertible_to_span<
    Dimensions, T,
    decltype(std::declval<span_converter<Dimensions, T>>().convert(std::declval<T>()), void())>
    : std::true_type {};

// Evaluates to true if <Dimensions, T> is a valid specialization of `convert_to_span`.
template <typename Dimensions, typename T>
constexpr bool is_convertible_to_span_v = is_convertible_to_span<Dimensions, T>::value;

// Evaluates to `true_type` if <Dimensions, T> is a valid specialization of `convert_to_span`, and
// the `convert` method is noexcept.
template <typename Dimensions, typename T>
struct is_nothrow_convertible_to_span {
  static constexpr bool value = is_convertible_to_span_v<Dimensions, T>&& noexcept(
      std::declval<span_converter<Dimensions, T>>().convert(std::declval<T>()));
};

// Evaluates to true if <Dimensions, T> is a valid specialization of `convert_to_span`, and
// the `convert` method is noexcept.
template <typename Dimensions, typename T>
constexpr bool is_nothrow_convertible_to_span_v =
    is_nothrow_convertible_to_span<Dimensions, T>::value;

}  // namespace detail

// Create an input span.
template <typename Dimensions, typename T>
constexpr auto make_input_span(const T& input) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack<Dimensions>::value, "Dimensions should be a value pack");
  static_assert(!std::is_same<T, std::nullptr_t>::value, "Input spans may not be null.");
  static_assert(detail::is_convertible_to_span<Dimensions, const T>::value,
                "The provided type does not have an implementation of: convert_to_span<T>");
  return detail::span_converter<Dimensions, T>{}.convert(input);
}

// Create a span for a required output argument.
template <typename Dimensions, typename T>
constexpr auto make_output_span(T& output) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack<Dimensions>::value, "Dimensions should be a value pack");
  static_assert(!std::is_same<T, std::nullptr_t>::value, "Required output spans may not be null.");
  static_assert(detail::is_convertible_to_span<Dimensions, T>::value,
                "The provided type does not have an implementation of: convert_to_span<T>");

  auto span = detail::span_converter<Dimensions, T>{}.convert(output);
  static_assert(!std::is_const<typename decltype(span)::value_type>::value,
                "value_type of output spans may not be const");
  return span;
}

// Create a span for an optional output argument.
template <typename Dimensions, typename T>
constexpr auto make_optional_output_span(T& output) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack<Dimensions>::value, "Dimensions should be a value pack");
  static_assert(detail::is_convertible_to_span<Dimensions, T>::value,
                "The provided type does not have an implementation of: convert_to_span<T>");

  auto span = detail::span_converter<Dimensions, T>{}.convert(output);
  static_assert(!std::is_const<typename decltype(span)::value_type>::value,
                "value_type of output spans may not be const");
  return span;
}

// Construct an input span w/ compile-time constant dimensions.
template <std::size_t... Dims, typename T>
constexpr auto make_input_span(const T& input) noexcept(
    detail::is_nothrow_convertible_to_span_v<constant_value_pack<Dims...>, T>) {
  return make_input_span<constant_value_pack<Dims...>>(input);
}

// Construct an  output span w/ compile-time constant dimensions.
template <std::size_t... Dims, typename T>
constexpr auto make_output_span(T& input) noexcept(
    detail::is_nothrow_convertible_to_span_v<constant_value_pack<Dims...>, T>) {
  return make_output_span<constant_value_pack<Dims...>>(input);
}

// Construct an optional output span w/ compile-time constant dimensions.
template <std::size_t... Dims, typename T>
constexpr auto make_optional_output_span(T& input) noexcept(
    detail::is_nothrow_convertible_to_span_v<constant_value_pack<Dims...>, T>) {
  return make_optional_output_span<constant_value_pack<Dims...>>(input);
}

// nullptr_t can be converted to a null span (for use in optional arguments).
template <typename Dimensions>
struct convert_to_span<Dimensions, std::nullptr_t> {
  constexpr auto convert(std::nullptr_t) noexcept { return make_always_null_span<Dimensions>(); }
};

// Spans are convertible to spans with identical dimensions.
// This allows the user to pass a span directly if they want, rather than specializing
// convert_to_span for their custom type.
// TODO: Allow conversion from dynamic -> constant dims with runtime checks.
template <typename Dimensions, typename T, typename DimsIn, typename StridesIn>
struct convert_to_span<Dimensions, span<T, DimsIn, StridesIn>,
                       typename std::enable_if<std::is_same<Dimensions, DimsIn>::value>::type> {
  constexpr span<T, DimsIn, StridesIn> convert(span<T, DimsIn, StridesIn> x) noexcept { return x; }
};

}  // namespace wf
