// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "span_detail.h"

namespace wf {

/**
 * Trait that implements conversion of user-provided type `T` to wf::span. The trait must
 * implement a single method, `convert(...)`, which accepts an instance of type `T` and returns a
 * span.
 *
 * An example implementation for a simple 2D matrix type with compile-time dimensions might look
 * something like:
 *
 * \code{.cpp}
 *    template <typename Dimensions, int Rows, int Cols>
 *    struct convert_to_span<Dimensions, MyMatrixType<Rows, Cols>> {
 *      // We employ a forwarding reference to accept both const and non-const.
 *      // If the type of `U` is const, we want to return a `span<const V, ...>`.
 *      template <typename U>
 *      constexpr auto convert(U&& matrix) const noexcept {
 *        // We'll assume the dimensions are always known at compile time. In practice, you may wish
 *        // to make this a runtime assertion if the matrix type is dynamic.
 *        static_assert(constant_value_pack_axis_v<0, Dimensions> == Rows);
 *        static_assert(constant_value_pack_axis_v<1, Dimensions> == Cols);
 *        // This example assumes row-major storage, so the stride between rows is `Cols`.
 *        // The stride between columns is 1 (each row is densely packed).
 *        constexpr auto strides = make_constant_value_pack<Cols, 1>();
 *        return make_span(matrix.data(), make_constant_value_pack<Rows, Cols>(), strides);
 *      }
 *    };
 * \endcode
 *
 * Scroll down this file for an example implementation that converts `Eigen::MatrixBase`-derived
 * classes to spans.
 *
 * @tparam Dimensions wf::value_pack describing the **expected** dimensions of the resulting span.
 * As of the time of this writing, these values are always compile-time constants.
 * @tparam T User-provided type being converted.
 *
 * @warning The user-provided specialization is ultimately responsible for checking that a specific
 * instance of `T` satisfies the values in `Dimensions`.
 */
template <typename Dimensions, typename T, typename = void>
struct convert_to_span;

// Fwd declare.
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_span(T* data, Dimensions dims, Strides strides) noexcept;

/**
 * A multidimensional span. Matrix and vector arguments to wrenfold functions are internally
 * converted to spans. Any user-provided vector or buffer can be passed to a wrenfold C++ function,
 * provided it implements the convert_to_span trait.
 *
 * @warning Typically you do not want construct this type directly. Instead, specialize
 * convert_to_span for whatever matrix/vector class you wish to support.
 *
 * @tparam T Numeric type of the underlying buffer (eg. float, double).
 * @tparam Dimensions value_pack describing the dimensions of the data.
 * @tparam Strides value_pack describing the strides of the data.
 */
template <typename T, typename Dimensions, typename Strides>
class span {
 public:
  static_assert(is_value_pack_v<Dimensions>, "Second template argument must be value_pack<...>");
  static_assert(is_value_pack_v<Strides>, "Third template argument must be value_pack<...>");
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

  /**
   * Number of dimensions in the span.
   */
  static constexpr std::size_t num_dimensions = Dimensions::length;

  // Construct from pointer and strides.
  constexpr span(T* data, Dimensions dims, Strides strides) noexcept
      : data_(data), dimensions_(dims), strides_(strides) {}

  /**
   * Permit implicit promotion from `span<T, ...>` to `span<const T, ....>`.
   *
   * @tparam U Non-const version of type `T`.
   *
   * @param s Source span to copy parameters from.
   */
  template <typename U, typename = detail::enable_if_adding_const_t<T, U>>
  constexpr span(const span<U, Dimensions, Strides>& s) noexcept  // NOLINT
      : span(const_cast<T*>(s.data()), s.dimensions(), s.strides()) {}

  /**
   * Number of rows (the dimension at index 0).
   */
  constexpr std::size_t rows() const noexcept { return dimensions_.template get<0>().value(); }

  /**
   * Number of columns (the dimension at index 1). Valid for spans with two or more dimensions.
   */
  constexpr std::size_t cols() const noexcept { return dimensions_.template get<1>().value(); }

  /**
   * Access value_pack of dimensions.
   */
  constexpr const Dimensions& dimensions() const noexcept { return dimensions_; }

  /**
   * Access value_pack of strides.
   */
  constexpr const Strides& strides() const noexcept { return strides_; }

  /**
   * Retrieve the size/shape of the span on a particular dimension.
   *
   * @tparam A Axis to retrieve.
   */
  template <std::size_t A>
  constexpr auto dimension() const noexcept {
    static_assert(A < num_dimensions, "dimension index is invalid");
    return dimensions_.template get<A>().value();
  }

  /**
   * Retrieve the stride of the spanned data on a particular dimension.
   *
   * @tparam A Axis to retrieve.
   */
  template <std::size_t A>
  constexpr auto stride() const noexcept {
    static_assert(A < num_dimensions, "dimension index is invalid");
    return strides_.template get<A>().value();
  }

  /**
   * Matrix-access operator.
   *
   * @warning No bounds checking is applied.
   *
   * @tparam Indices These must be integral values that are convertible to `std::ptrdiff_t`.
   *
   * @param indices Indices into the underlying data. For a 2D span, this will be a (row, column)
   * pair.
   *
   * @return A reference to an element in the span.
   */
  template <typename... Indices>
  constexpr reference operator()(Indices... indices) const noexcept {
    return data_[compute_index(indices...)];
  }

  /**
   * Array access operator. Valid only for 1D spans.
   *
   * @warning No bounds checking is applied.
   *
   * @param index Element to access.
   *
   * @return A reference to an element in the span.
   */
  constexpr reference operator[](const std::ptrdiff_t index) const noexcept {
    return data_[compute_index(index)];
  }

  // Compute linear index from (row, column, ...) matrix indices.
  template <typename... Indices>
  constexpr std::ptrdiff_t compute_index(Indices... indices) const noexcept {
    static_assert(detail::conjunction_v<std::is_convertible<Indices, std::ptrdiff_t>...>,
                  "Indices must be convertible to std::ptrdiff_t");
    static_assert(sizeof...(Indices) == num_dimensions,
                  "Number of indices much match number of dimensions");
    return compute_index_internal(std::make_integer_sequence<std::size_t, num_dimensions>(),
                                  indices...);
  }

  /**
   * Pointer to the start of the underlying data.
   */
  constexpr pointer data() const noexcept { return data_; }

  /**
   * Implicit conversion to bool. Evaluates to true if the underlying data pointer is non-null.
   */
  constexpr operator bool() const noexcept { return data_ != nullptr; }  // NOLINT

  /**
   * Create a span with the same underlying dimensions and strides as `this`, but with a const
   * data type.
   *
   * @return A new span of the form `span<const T, ...>`.
   */
  constexpr span<const std::remove_const_t<T>, Dimensions, Strides> as_const() const noexcept {
    return {data_, dimensions(), strides()};
  }

  /**
   * Create a new span referencing a sub-block of `this`.
   *
   * @warning No checks are employed to enforce that the new span does not exceed the bounds of the
   * original.
   *
   * @tparam O A wf::value_pack whose dimensions match those of this span.
   * @tparam D A wf::value_pack whose dimensions match those of this span.
   *
   * @param offsets Where the block starts.
   * @param dims The size of the block on each exis.
   *
   * @return A new span with the same strides as the original, but with a new starting address and
   * dimensions of type `D`.
   */
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

/**
 * Construct a span from a pointer, dimensions, and strides.
 *
 * Example usage:
 * \code{.cpp}
 *   // Make a span over a fixed-size array. The data is assumed to have a shape of 4x6, in
 *   // column-major order.
 *   std::array<double, 24> buffer{};
 *   const auto span = make_span(buffer.data(), make_constant_value_pack<4, 6>(),
 *                               make_constant_value_pack<1, 6>());
 * \endcode
 *
 * @tparam T Type of the underlying data (eg. float, double).
 * @tparam Dimensions A wf::value_pack.
 * @tparam Strides A wf::value_pack, the length of which should match `Dimensions`.
 *
 * @param data Pointer to the start of the spanned data.
 * @param dims The shape of the data.
 * @param strides The stride of the spanned data.
 *
 * @return A new span.
 */
template <typename T, typename Dimensions, typename Strides>
constexpr auto make_span(T* data, Dimensions dims, Strides strides) noexcept {
  using dimension_type = std::decay_t<Dimensions>;
  using stride_type = std::decay_t<Strides>;
  return span<T, dimension_type, stride_type>{data, dims, strides};
}

/**
 * Create a null span with the specified dimensions. Null spans can be passed to optional output
 * arguments in generated functions.
 *
 * @tparam Dimensions A wf::value_pack of compile-time integrals.
 *
 * @return A span with a null data pointer.
 */
template <typename Dimensions>
constexpr auto make_always_null_span() noexcept {
  static_assert(is_value_pack_v<Dimensions>, "Dimensions must be a value_pack");
  return make_span<detail::void_type>(nullptr, Dimensions::zero_initialized(),
                                      detail::make_zero_value_pack<Dimensions::length>());
}

/**
 * Create a 1D span from an array-like object. An array-like object meets the following criteria:
 *   - Exposes a `data()` function that returns a pointer.
 *   - Exposes a `size()` function that returns an integral value.
 *
 * For example, `std::vector` satifies these criteria.
 *
 * @tparam T A type satisfying `enable_if_array_like_t`
 *
 * @param array It is assumed the data in `array` is layed out densely (ie. stride of 1).
 *
 * @return A new span.
 *
 * @todo In some cases (`std::array` for example) the span could have compile-time length. This
 * has yet to be implemented.
 */
template <typename T, typename = detail::enable_if_array_like_t<T>>
constexpr auto make_array_span(T&& array) noexcept {
  const dynamic dim{array.size()};
  return make_span(array.data(), make_value_pack(dim), make_constant_value_pack<1>());
}

/**
 * Create a 1D span from a C-style array.
 *
 * @tparam T Type of the elements in the array.
 * @tparam N Size of the array. Must be at least one.
 *
 * @param array It is assumed the data in `array` is layed out densely (ie. stride of 1).
 *
 * @return A new span.
 */
template <typename T, std::size_t N>
constexpr auto make_array_span(T (&array)[N]) noexcept {
  static_assert(N > 0, "Array must have at least one element.");
  return make_span(&array[0], make_constant_value_pack<N>(), make_constant_value_pack<1>());
}

namespace detail {

// Remove reference and const modifiers.
template <typename T>
using remove_const_and_ref_t = std::remove_const_t<std::remove_reference_t<T>>;

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

/**
 * Create an input span. This method will instantiate `convert_to_span<Dimensions, T>` and invoke
 * the member function `convert(input)`.
 *
 * @tparam Dimensions wf::value_pack of compile-time values indicating the expected dimensions of
 * the resultant span.
 *
 * @param input Object whose data will be accessed through the underlying span. Input spans must
 * point to valid data - behavior is undefined if this object is null/invalid.
 *
 * @return A new immutable span.
 */
template <typename Dimensions, typename T>
constexpr auto make_input_span(const T& input) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack_v<Dimensions>, "Dimensions should be a value pack");
  static_assert(!std::is_same_v<T, std::nullptr_t>, "Input spans may not be null.");
  static_assert(detail::is_convertible_to_span_v<Dimensions, const T>,
                "The provided type does not have an implementation of: convert_to_span<T>");
  return detail::span_converter<Dimensions, T>{}.convert(input);
}

/**
 * Create a span for an output argument. This method will instantiate `convert_to_span<Dimensions,
 * T>` and invoke the member function `convert(input)`.
 *
 * @tparam Dimensions wf::value_pack of compile-time values indicating the expected dimensions of
 * the resultant span.
 *
 * @param output Object whose data will be accessed through the underlying span. Output spans must
 * point to valid data - behavior is undefined if this object is null/invalid.
 *
 * @return A new mutable span.
 */
template <typename Dimensions, typename T>
constexpr auto make_output_span(T& output) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack_v<Dimensions>, "Dimensions should be a value pack");
  static_assert(!std::is_same_v<T, std::nullptr_t>, "Required output spans may not be null.");
  static_assert(detail::is_convertible_to_span_v<Dimensions, T>,
                "The provided type does not have an implementation of: convert_to_span<T>");

  auto span = detail::span_converter<Dimensions, T>{}.convert(output);
  static_assert(!std::is_const_v<typename decltype(span)::value_type>,
                "value_type of output spans may not be const");
  return span;
}

/**
 * Create a span for an optional output argument. This method will instantiate
 * `convert_to_span<Dimensions, T>` and invoke the member function `convert(input)`. Unlike input
 * and output spans, optional output spans may be null.
 *
 * @tparam Dimensions wf::value_pack of compile-time values indicating the expected dimensions of
 * the resultant span.
 *
 * @param output Object whose data will be accessed through the underlying span.
 *
 * @return A new mutable span.
 */
template <typename Dimensions, typename T>
constexpr auto make_optional_output_span(T& output) noexcept(
    detail::is_nothrow_convertible_to_span_v<Dimensions, T>) {
  static_assert(is_value_pack_v<Dimensions>, "Dimensions should be a value pack");
  static_assert(detail::is_convertible_to_span_v<Dimensions, T>,
                "The provided type does not have an implementation of: convert_to_span<T>");

  auto span = detail::span_converter<Dimensions, T>{}.convert(output);
  static_assert(!std::is_const_v<typename decltype(span)::value_type>,
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
                       std::enable_if_t<std::is_same_v<Dimensions, DimsIn>>> {
  constexpr span<T, DimsIn, StridesIn> convert(span<T, DimsIn, StridesIn> x) noexcept { return x; }
};

}  // namespace wf

// Begin optional Eigen Support:
// -----------------------------
// Define `WF_SPAN_EIGEN_SUPPORT` prior to including this header to enable passing of eigen types
// to generated functions.
#ifdef WF_SPAN_EIGEN_SUPPORT
#include <Eigen/Core>
#include <Eigen/Geometry>  //  QuaternionBase

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
using eigen_stride_type_t = std::conditional_t<Stride == Eigen::Dynamic, dynamic,
                                               constant<static_cast<std::size_t>(Stride)>>;

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
using enable_if_inherits_matrix_base_t = std::enable_if_t<inherits_matrix_base_v<std::decay_t<T>>>;

// Evaluates to std::true_type if `T` inherits from QuaternionBase, otherwise std::false_type.
template <typename T>
using inherits_quaternion_base =
    decltype(detail::inherits_quaternion_base_(std::declval<const T>()));
template <typename T>
constexpr bool inherits_quaternion_base_v = inherits_quaternion_base<T>::value;

// Evaluates to `void` if `T` inherits from QuaternionBase.
template <typename T>
using enable_if_inherits_quaternion_base_t =
    std::enable_if_t<inherits_quaternion_base_v<std::decay_t<T>>>;

// Enable conversion of `MatrixBase` children to spans.
// Eigen::Map is not checked for nullptr, which allows passing empty maps for optional output spans.
template <typename Dimensions, typename T>
struct convert_to_span<Dimensions, T, enable_if_inherits_matrix_base_t<T>> {
  template <typename U>
  constexpr auto convert(U&& mat) const noexcept {
    using UDecay = std::decay_t<U>;

    constexpr Eigen::Index rows_at_compile_time = Eigen::DenseBase<UDecay>::RowsAtCompileTime;
    constexpr Eigen::Index cols_at_compile_time = Eigen::DenseBase<UDecay>::ColsAtCompileTime;
    static_assert(rows_at_compile_time != Eigen::Dynamic, "Rows must be known at compile time");
    static_assert(cols_at_compile_time != Eigen::Dynamic, "Cols must be known at compile time");

    // Check that dimensions agree with what the generated method expects.
    static_assert(
        static_cast<std::size_t>(rows_at_compile_time) == constant_value_pack_axis_v<0, Dimensions>,
        "Number of rows does not match expected dimension");
    static_assert(
        static_cast<std::size_t>(cols_at_compile_time) == constant_value_pack_axis_v<1, Dimensions>,
        "Number of columns does not match expected dimension");

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
  constexpr auto convert(U&& q) const noexcept {
    // Quaternion is convertible to 4x1 column vector.
    constexpr auto dims = make_constant_value_pack<4, 1>();
    constexpr auto strides = make_constant_value_pack<1, 4>();
    return make_span(q.coeffs().data(), dims, strides);
  }
};

}  // namespace wf

#endif  // WF_SPAN_EIGEN_SUPPORT
