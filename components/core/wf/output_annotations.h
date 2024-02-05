#pragma once
#include <string>
#include <type_traits>

namespace wf {

// Denote a captured result from a function that should be mapped to an output argument.
// Stores the symbolic expression, a variable name, and whether the output should be optional.
template <typename T>
class output_arg {
 public:
  using value_type = T;

  template <typename U>
  using enable_if_constructible_t = std::enable_if_t<std::is_constructible_v<T, U>>;

  template <typename U, typename = enable_if_constructible_t<U>>
  constexpr output_arg(const std::string_view name, U&& u)
      : value_(std::forward<U>(u)), name_(name), is_optional_(false) {}

  template <typename U, typename = enable_if_constructible_t<U>>
  constexpr output_arg(const std::string_view name, U&& u, const bool is_optional)
      : value_(std::forward<U>(u)), name_(name), is_optional_(is_optional) {}

  // Access the output value. Typically, an `scalar_expr` or `matrix_expr`, etc.
  constexpr const T& value() const { return value_; }

  // Name of the argument.
  constexpr const std::string& name() const { return name_; }

  // True if the output argument is optional.
  constexpr bool is_optional() const { return is_optional_; }

 private:
  T value_;
  std::string name_;
  bool is_optional_;
};

// Deduction guide for `output_arg`.
template <class T>
output_arg(std::string_view name, T&& value) -> output_arg<std::decay_t<T>>;

// Construct an output argument and designate it as optional.
template <typename T>
constexpr auto optional_output_arg(std::string_view name, T&& value) {
  return output_arg<std::decay_t<T>>(name, std::forward<T>(value), true);
}

// Denote a captured result from a function that should be mapped to a return value.
// Return values can never be optional.
template <typename T>
class return_value {
 public:
  using value_type = T;

  template <typename U>
  using enable_if_constructible_t = std::enable_if_t<std::is_constructible_v<T, U>>;

  template <typename U, typename = enable_if_constructible_t<U>>
  explicit return_value(U&& value) : value_(std::forward<U>(value)) {}

  // Access the output value. Typically, an `scalar_expr` or `matrix_expr`, etc.
  constexpr const T& value() const { return value_; }

  // Convert to `output_arg`.
  output_arg<T> to_output_arg(const std::string_view name) const {
    return output_arg(name, value_);
  }

 private:
  T value_;
};

// Deduction guide for `return_value`.
template <class T>
return_value(T&& value) -> return_value<std::decay_t<T>>;

template <typename T>
struct is_output_arg : std::false_type {};
template <typename T>
struct is_output_arg<output_arg<T>> : std::true_type {};

template <typename T>
struct is_return_value : std::false_type {};
template <typename T>
struct is_return_value<return_value<T>> : std::true_type {};

template <typename T>
struct is_output_arg_or_return_value : std::disjunction<is_output_arg<T>, is_return_value<T>> {};
template <typename T>
constexpr bool is_output_arg_or_return_value_v = is_output_arg_or_return_value<T>::value;

// Count the number of instances of `return_value` in a type pack.
template <typename... Ts>
constexpr std::size_t count_return_values_v =
    (static_cast<std::size_t>(is_return_value<Ts>::value) + ...);

namespace detail {
template <std::ptrdiff_t Index>
constexpr std::ptrdiff_t return_value_index_recursive() {
  return -1;
}
template <std::ptrdiff_t Index, typename T, typename... Ts>
constexpr std::ptrdiff_t return_value_index_recursive() {
  return is_return_value<T>::value ? Index : return_value_index_recursive<Index + 1, Ts...>();
}
}  // namespace detail

// Get the index of the first `return_value` type in a type pack or tuple by recursively searching
// the list of types. Yields -1 if there is no return value.
template <typename T>
struct return_value_index;
template <typename... Ts>
struct return_value_index<std::tuple<Ts...>> {
  static constexpr std::ptrdiff_t value = detail::return_value_index_recursive<0, Ts...>();
};

}  // namespace wf
