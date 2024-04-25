// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <tuple>
#include <type_traits>

namespace wf {

/**
 * A length or stride value that is known at compile-time.
 *
 * @tparam D Dimension/stride.
 */
template <std::size_t D>
class constant {
 public:
  constexpr constant() noexcept = default;

  // It is easier to write variadic utility functions if we define this constructor.
  // The passed value is dropped, since `constant` does not store anything.
  explicit constexpr constant(std::size_t) noexcept {}

  /**
   * Retrieve the value.
   */
  static constexpr std::size_t value() noexcept { return D; }
};

/**
 * Store a length or stride value that is determined at runtime.
 */
class dynamic {
 public:
  // Construct with stride value.
  explicit constexpr dynamic(const std::size_t value) noexcept : value_(value) {}

  /**
   * Retrieve the value.
   */
  constexpr std::size_t value() const noexcept { return value_; }

 private:
  std::size_t value_;
};

namespace detail {

// Implementation of conjunction.
template <typename...>
struct conjunction : std::true_type {};
template <typename T, typename... Ts>
struct conjunction<T, Ts...> : std::conditional_t<T::value, conjunction<Ts...>, std::false_type> {};
template <typename... Ts>
constexpr bool conjunction_v = conjunction<Ts...>::value;

// Evaluates to true if `T` is an instance of `constant<D>`
template <typename T>
struct is_constant : std::false_type {};
template <std::size_t D>
struct is_constant<constant<D>> : std::true_type {};

// Represents a variadic list of values when all values are instances of `constant<>`.
// Does not store anything, since the values are knowable at compile time.
template <typename... Values>
class value_pack_const {
 public:
  static_assert(detail::conjunction_v<is_constant<Values>...>,
                "All values must be compile-time constants");

  using tuple_type = std::tuple<Values...>;

  // Default construct.
  explicit constexpr value_pack_const() noexcept = default;

  // Construct from values.
  explicit constexpr value_pack_const(Values...) noexcept {}

  // Get the value on axis `A`.
  template <std::size_t A>
  constexpr std::tuple_element_t<A, tuple_type> get_axis() const noexcept {
    static_assert(A < sizeof...(Values), "Invalid dimension index");
    return std::tuple_element_t<A, tuple_type>{};
  }

  // Access all the values as a tuple.
  constexpr std::tuple<Values...> values() const noexcept { return std::make_tuple(Values{}...); }
};

// Stores a variadic list of values when some values are `constant<>` and others `dynamic`.
template <typename... Values>
class value_pack_dynamic {
 public:
  using tuple_type = std::tuple<Values...>;

  // Construct from `Values`, which may be dynamic or constant structs.
  explicit constexpr value_pack_dynamic(Values... values) noexcept : values_{values...} {}

  // Get the value on axis `A`.
  template <std::size_t A>
  constexpr std::tuple_element_t<A, tuple_type> get_axis() const noexcept {
    static_assert(A < sizeof...(Values), "Invalid dimension index");
    return std::get<A>(values_);
  }

  // Access all the values as a tuple.
  constexpr const tuple_type& values() const noexcept { return values_; }

 private:
  tuple_type values_;
};

}  // namespace detail

/**
 * Store a sequence of values, each of which represents the size or stride along a partiular axis
 * of a span.
 *
 * @tparam Values Variadic list of wf::constant or wf::dynamic values.
 */
template <typename... Values>
class value_pack : public std::conditional_t<detail::conjunction_v<detail::is_constant<Values>...>,
                                             detail::value_pack_const<Values...>,
                                             detail::value_pack_dynamic<Values...>> {
 public:
  static_assert(sizeof...(Values) > 0, "Must have at least one dimension");

  /**
   * True if all values are compile-time constants.
   */
  static constexpr bool known_at_compile_time =
      detail::conjunction_v<detail::is_constant<Values>...>;

  /**
   * Number of values in this pack.
   */
  static constexpr std::size_t length = sizeof...(Values);

  // The base class, which differs if all dimensions are known at compile time.
  using Base = std::conditional_t<known_at_compile_time, detail::value_pack_const<Values...>,
                                  detail::value_pack_dynamic<Values...>>;

  // Include constructor from the base type.
  using Base::Base;

  // Create a zero-initialized value pack.
  static constexpr value_pack zero_initialized() noexcept { return value_pack(Values(0)...); }

  /**
   * Access value for a particular dimension.
   *
   * @tparam A Index of the element to retrieve.
   */
  template <std::size_t A>
  constexpr auto get() const noexcept {
    return Base::template get_axis<A>();
  }
};

/**
 * Alias for a value_pack of wf::constant values.
 *
 * @tparam IJK Parameter pack of indices to be converted to wf::constant.
 */
template <std::size_t... IJK>
using constant_value_pack = value_pack<constant<IJK>...>;

// Evaluates to true if `T` is an instance of `value_pack<...>`.
template <typename T>
struct is_value_pack : std::false_type {};
template <typename... Values>
struct is_value_pack<value_pack<Values...>> : std::true_type {};
template <typename T>
constexpr bool is_value_pack_v = is_value_pack<T>::value;

// Evaluates to true if `T` is a value_pack of compile-time values.
template <typename T>
struct is_constant_value_pack;
template <typename... Values>
struct is_constant_value_pack<value_pack<Values...>>
    : detail::conjunction<detail::is_constant<Values>...> {};
template <typename T>
constexpr bool is_constant_value_pack_v = is_constant_value_pack<T>::value;

// Get element `D` from the sequence of integers [I, JK...]
template <std::size_t D, std::size_t I, std::size_t... JK>
struct index_sequence_value : index_sequence_value<D - 1, JK...> {};
template <std::size_t I, std::size_t... JK>
struct index_sequence_value<0, I, JK...> : std::integral_constant<std::size_t, I> {};

// Get dimension `D` of a compile-time constant value_pack.
template <std::size_t D, typename T>
struct constant_value_pack_axis;
template <std::size_t D, std::size_t... IJK>
struct constant_value_pack_axis<D, value_pack<constant<IJK>...>> {
  static constexpr std::size_t value = index_sequence_value<D, IJK...>::value;
};
template <std::size_t D, typename T>
constexpr auto constant_value_pack_axis_v = constant_value_pack_axis<D, T>::value;

// Get the number of dimensions in a value pack.
template <typename T>
struct value_pack_length;
template <typename... Values>
struct value_pack_length<value_pack<Values...>> {
  static constexpr std::size_t value = sizeof...(Values);
};
template <typename T>
constexpr std::size_t value_pack_length_v = value_pack_length<T>::value;

namespace detail {

// Template for converting a type `T` to either `dynamic` or `constant`.
template <typename T, typename = void>
struct convert_to_dimension_type;
template <typename T>
using convert_to_dimension_type_t = typename convert_to_dimension_type<T>::type;

// dynamic and constant are already dimension types and require no conversion.
template <>
struct convert_to_dimension_type<dynamic> {
  using type = dynamic;
  static constexpr auto convert(const dynamic x) noexcept { return x; }
};
template <std::size_t D>
struct convert_to_dimension_type<constant<D>> {
  using type = constant<D>;
  static constexpr auto convert(constant<D>) noexcept { return type{}; }
};

// Allow promotion of integral types to `dynamic`.
template <typename T>
struct convert_to_dimension_type<
    T, std::enable_if_t<std::is_integral_v<T> && std::is_convertible_v<T, std::size_t>>> {
  using type = dynamic;
  static constexpr auto convert(T value) noexcept {
    return dynamic(static_cast<std::size_t>(value));
  }
};

}  // namespace detail

/**
 * Construct a value_pack from variadic arguments.
 *
 * Example usage:
 * \code{.cpp}
 *   // Create dimensions for a 2x8x3 sized buffer.
 *   const auto dims = make_value_pack(constant<2>{}, 8, dynamic(3));
 * \endcode
 *
 * @param values These may be wf::constant compile-time values, wf::dynamic runtime values, or
 * integrals that will automatically be promoted to wf::dynamic.
 *
 * @return Instance of value_pack with the specified values.
 */
template <typename... Values>
constexpr auto make_value_pack(Values... values) noexcept {
  return value_pack<detail::convert_to_dimension_type_t<Values>...>{
      detail::convert_to_dimension_type<Values>::convert(values)...};
}

/**
 * Construct a value_pack from a template parameter pack of std::size_t values.
 *
 * Example usage:
 * \code{.cpp}
 *   // Create dimensions for a 4x2 sized buffer.
 *   const auto dims = make_constant_value_pack<4, 2>();
 * \endcode
 *
 * @tparam Dims Values to place in the value_pack.
 *
 * @return value_pack of wf::constant
 */
template <std::size_t... Dims>
constexpr auto make_constant_value_pack() noexcept {
  return make_value_pack(constant<Dims>{}...);
}

namespace detail {

// Enable if: `T` is the const version of `U`.
template <typename T, typename U>
using enable_if_adding_const_t =
    std::enable_if_t<std::is_const_v<T> && std::is_same_v<std::remove_const_t<T>, U>>;

// Enable if `T` is the same as `U` after const is removed from both.
template <typename T, typename U>
using enable_if_same_after_removing_const_t =
    std::enable_if_t<std::is_same_v<std::remove_const_t<T>, std::remove_const_t<U>>>;

// True if `T` has a method size() that returns an integral type.
template <typename T, typename = void>
struct has_size_method : std::false_type {};
template <typename T>
struct has_size_method<T, decltype(std::declval<const T>().size(), void())>
    : std::is_integral<decltype(std::declval<const T>().size())> {};

// True if `T` has a method data() that returns a pointer.
template <typename T, typename = void>
struct has_data_method : std::false_type {};
template <typename T>
struct has_data_method<T, decltype(std::declval<const T>().data(), void())>
    : std::is_pointer<decltype(std::declval<const T>().data())> {};

// True if `T` is "array-like" (has data() and size()).
template <typename T>
using enable_if_array_like_t =
    std::enable_if_t<has_data_method<T>::value && has_size_method<T>::value>;

// Create a `strides` object w/ all zero strides. The dimensionality is determined by the integer
// sequence.
template <std::size_t... Counter>
constexpr auto make_zero_value_pack(std::integer_sequence<std::size_t, Counter...>) noexcept {
  return make_value_pack(constant<0>(Counter & 0)...);
}
template <std::size_t N>
constexpr auto make_zero_value_pack() noexcept {
  return make_zero_value_pack(std::make_integer_sequence<std::size_t, N>());
}

// A type that does nothing, and can be assigned from anything.
struct void_type {
  template <typename T>
  constexpr void_type& operator=(T&&) {
    return *this;
  }
};

}  // namespace detail
}  // namespace wf
