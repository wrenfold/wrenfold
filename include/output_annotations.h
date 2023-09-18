#pragma once
#include <string>
#include <type_traits>

namespace math {

// Denote a captured result from a function that should be mapped to an output argument.
// Stores the symbolic expression, a variable name, and whether the output should be optional.
template <typename T>
class OutputArg {
 public:
  template <typename U>
  using EnableIfDecayedTypeMatches = std::enable_if_t<std::is_same_v<T, std::decay_t<U>>>;

  template <typename U, typename = EnableIfDecayedTypeMatches<U>>
  constexpr OutputArg(std::string_view name, U&& u)
      : value_(std::forward<U>(u)), name_(name), is_optional_(false) {}

  template <typename U, typename = EnableIfDecayedTypeMatches<U>>
  constexpr OutputArg(std::string_view name, U&& u, bool is_optional)
      : value_(std::forward<U>(u)), name_(name), is_optional_(is_optional) {}

  // Access the output value. Typically, an `Expr` or `MatrixExpr`, etc.
  constexpr const T& Value() const { return value_; }

  // Name of the argument.
  constexpr const std::string& Name() const { return name_; }

  // True if the output argument is optional.
  constexpr bool IsOptional() const { return is_optional_; }

 private:
  T value_;
  std::string name_;
  bool is_optional_;
};

// Deduction guide for `OutputArg`.
template <class T>
OutputArg(std::string_view name, T&& value) -> OutputArg<std::decay_t<T>>;

// Construct an output argument and designate it as optional.
template <typename T>
constexpr auto OptionalOutputArg(std::string_view name, T&& value) {
  return OutputArg<std::decay_t<T>>(name, std::forward<T>(value), true);
}

// Denote a captured result from a function that should be mapped to a return value.
// Return values can never be optional.
template <typename T>
class ReturnValue {
 public:
  template <typename U>
  using EnableIfDecayedTypeMatches = std::enable_if_t<std::is_same_v<T, std::decay_t<U>>>;

  template <typename U, typename = EnableIfDecayedTypeMatches<U>>
  explicit ReturnValue(U&& value) : value_(std::forward<U>(value)) {}

  // Access the output value. Typically, an `Expr` or `MatrixExpr`, etc.
  constexpr const T& Value() const { return value_; }

  // Convert to `OutputArg`.
  OutputArg<T> ToOutputArg(const std::string_view name) const { return OutputArg(name, value_); }

 private:
  T value_;
};

// Deduction guide for `ReturnValue`.
template <class T>
ReturnValue(T&& value) -> ReturnValue<std::decay_t<T>>;

template <typename T>
struct IsOutputArg : public std::false_type {};
template <typename T>
struct IsOutputArg<OutputArg<T>> : public std::true_type {};

template <typename T>
struct IsReturnValue : public std::false_type {};
template <typename T>
struct IsReturnValue<ReturnValue<T>> : public std::true_type {};

template <typename T>
struct IsOutputArgOrReturnValue : public std::disjunction<IsOutputArg<T>, IsReturnValue<T>> {};

// Count the number of instances of `ReturnValue` in a type pack.
template <typename... Ts>
constexpr std::size_t CountReturnValues =
    (static_cast<std::size_t>(IsReturnValue<Ts>::value) + ...);

namespace detail {
template <std::ptrdiff_t Index>
constexpr std::ptrdiff_t ReturnValueIndexRecursive() {
  return -1;
}
template <std::ptrdiff_t Index, typename T, typename... Ts>
constexpr std::ptrdiff_t ReturnValueIndexRecursive() {
  return IsReturnValue<T>::value ? Index : ReturnValueIndexRecursive<Index + 1, Ts...>();
}
}  // namespace detail

// Get the index of the first `ReturnValue` type in a type pack or tuple by recursively searching
// the list of types. Yields -1 if there is no return value.
template <typename T>
struct ReturnValueIndex;
template <typename... Ts>
struct ReturnValueIndex<std::tuple<Ts...>> {
  static constexpr std::ptrdiff_t value = detail::ReturnValueIndexRecursive<0, Ts...>();
};

}  // namespace math
