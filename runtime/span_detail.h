// Copyright 2023 Gareth Cross
#pragma once
#include <tuple>

namespace math {

// Represent a compile-time constant dimension or stride value.
template <std::size_t D>
class constant {
 public:
  constexpr constant() noexcept = default;

  // It is easier to write variadic utility functions if we define this constructor.
  // The passed value is dropped, since `constant` does not store anything.
  explicit constexpr constant(std::size_t) noexcept {};

  static constexpr std::size_t value() noexcept { return D; }
};

// Represent a run-time dimension or stride value.
class dynamic {
 public:
  // Construct with stride value.
  explicit constexpr dynamic(std::size_t value) noexcept : value_(value) {}

  // Access the stride value.
  constexpr std::size_t value() const noexcept { return value_; }

 private:
  std::size_t value_;
};

namespace detail {

// Evaluates to true if `T` is an instance of `constant<D>`
template <typename T>
struct is_constant : public std::false_type {};
template <std::size_t D>
struct is_constant<constant<D>> : public std::true_type {};

// Enable if all the `Ints` are convertible to ptrdiff_t.
template <typename... Ints>
using enable_if_convertible_to_ptrdiff_t = typename std::enable_if<
    std::conjunction<std::is_convertible<Ints, std::ptrdiff_t>...>::value>::type;

// Represents a variadic list of values when all values are instances of `constant<>`.
// Does not store anything, since the values are knowable at compile time.
template <typename... Values>
class value_pack_const {
 public:
  static_assert(std::conjunction<is_constant<Values>...>::value,
                "All values must be compile-time constants");

  // Construct from values.
  explicit constexpr value_pack_const(Values...) noexcept {}

  // Get the value on axis `D`.
  template <std::size_t D>
  constexpr auto get_axis() const noexcept {
    static_assert(D < sizeof...(Values), "Invalid dimension index");
    using tuple_type = std::tuple<Values...>;
    return std::tuple_element_t<D, tuple_type>{};
  }
};

// Stores a variadic list of values when some values are `constant<>` and others `dynamic`.
template <typename... Values>
class value_pack_dynamic {
 public:
  // Construct from `Values`, which may be dynamic or constant structs.
  explicit constexpr value_pack_dynamic(Values... values) noexcept : values_{values...} {}

  // Get the value on axis `D`.
  // TODO: This cannot be constexpr until c++14 because of std::get<>.
  template <std::size_t D>
  constexpr auto get_axis() const noexcept {
    static_assert(D < sizeof...(Values), "Invalid dimension index");
    return std::get<D>(values_);
  }

 private:
  // TODO: Only store the dynamic ones for space saving?
  std::tuple<Values...> values_;
};

}  // namespace detail

// Inherits either `value_pack_const` or `value_pack_dynamic`, depending on the types of `Values`.
template <typename... Values>
class value_pack : public std::conditional<std::conjunction<detail::is_constant<Values>...>::value,
                                           detail::value_pack_const<Values...>,
                                           detail::value_pack_dynamic<Values...>>::type {
 public:
  static_assert(sizeof...(Values) > 0, "Must have at least one dimension");

  // True if all values are known at compile time.
  static constexpr bool known_at_compile_time =
      std::conjunction<detail::is_constant<Values>...>::value;

  // Number of dimensions.
  static constexpr std::size_t length = sizeof...(Values);

  // Is this a one dimensional span.
  static constexpr bool is_one_dimensional = length == 1;

  // The base class, which differs if all dimensions are known at compile time.
  using Base = typename std::conditional<known_at_compile_time, detail::value_pack_const<Values...>,
                                         detail::value_pack_dynamic<Values...>>::type;

  // Include constructor from the base type.
  using Base::Base;

  template <std::size_t D>
  constexpr auto get() const noexcept {
    return Base::template get_axis<D>();
  }
};

namespace detail {

// Evaluates to true if `T` is an instance of `value_pack<...>`.
template <typename T>
struct is_value_pack : public std::false_type {};
template <typename... Values>
struct is_value_pack<value_pack<Values...>> : public std::true_type {};

}  // namespace detail

// Construct `dimensions` from variadic args.
template <typename... Values>
auto make_value_pack(Values&&... values) {
  return value_pack<typename std::decay<Values>::type...>{std::forward<Values>(values)...};
}

// Construct compile-time constant dimensions from template parameters.
template <std::size_t... Dims>
auto make_constant_value_pack() {
  return make_value_pack(constant<Dims>{}...);
}

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

// True if `T` has a method size() that returns an integral type.
template <typename T, typename = void>
struct has_size_method : public std::false_type {};
template <typename T>
struct has_size_method<T, decltype(std::declval<const T>().size(), void())>
    : public std::is_integral<decltype(std::declval<const T>().size())> {};

// True if `T` has a method data() that returns a pointer.
template <typename T, typename = void>
struct has_data_method : public std::false_type {};
template <typename T>
struct has_data_method<T, decltype(std::declval<const T>().data(), void())>
    : public std::is_pointer<decltype(std::declval<const T>().data())> {};

// True if `T` is "array-like" (has data() and size()).
template <typename T>
using enable_if_array_like_t =
    typename std::enable_if<has_data_method<T>::value && has_size_method<T>::value>::type;

// Create a `strides` object w/ all zero strides. The dimensionality is determined by the integer
// sequence.
template <std::size_t... Counter>
constexpr auto make_zero_strides(std::integer_sequence<std::size_t, Counter...>) noexcept {
  return make_value_pack(constant<0>(Counter & 0)...);
}
template <std::size_t N>
constexpr auto make_zero_strides() noexcept {
  return make_zero_strides(std::make_integer_sequence<std::size_t, N>());
}

// Use a placeholder type for selecting the unchecked constructor of not_null_span.
struct unchecked {};

}  // namespace detail

}  // namespace math
