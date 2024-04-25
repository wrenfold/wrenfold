// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <limits>

#include "wf/error_types.h"
#include "wf/hashing.h"

namespace wf {

namespace detail {

// Check if `Src` is a signed int that fits with `Dest`.
template <typename Dest, typename Src, typename = void>
struct allows_implicit_conversion : std::false_type {};

// Both types are signed, so we can compare their ranges (promotion will occur).
template <typename Dest, typename Src>
struct allows_implicit_conversion<
    Dest, Src, std::enable_if_t<std::is_integral_v<Src> && std::is_signed_v<Src>>>
    : std::conditional_t<std::numeric_limits<Src>::min() >= std::numeric_limits<Dest>::min() &&
                             std::numeric_limits<Src>::max() <= std::numeric_limits<Dest>::max(),
                         std::true_type, std::false_type> {};

template <typename Dest, typename Src>
constexpr bool allows_implicit_conversion_v = allows_implicit_conversion<Dest, Src>::value;

}  // namespace detail

// A 64-bit signed integer type that checks operations at runtime, and throws on invalid operations.
// Evidently, runtime checking will come at a performance cost, but we need to catch overflows
// during symbolic manipulation. This class assumes a two's complement representation.
class checked_int final {
 public:
  using value_type = std::int64_t;

  template <typename Src, typename U = void>
  using enable_if_allows_implicit_conversion_v =
      std::enable_if_t<detail::allows_implicit_conversion_v<value_type, Src>, U>;

  // Allow implicit construction from signed integers of same or smaller range.
  template <typename T, typename = enable_if_allows_implicit_conversion_v<T>>
  constexpr checked_int(T v) noexcept : value_(static_cast<value_type>(v)) {}  //  NOLINT

  // Access underlying value.
  constexpr value_type value() const noexcept { return value_; }

  // Explicit cast to `value_type`.
  explicit constexpr operator value_type() const noexcept { return value_; }

  // Cast to double. (No check for precision loss).
  explicit constexpr operator double() const noexcept { return static_cast<double>(value_); }

  // Cast to equivalently sized unsigned int, with check for range.
  explicit constexpr operator std::uint64_t() const {
    if (value_ < 0) {
      throw domain_error("Invalid cast of negative value {} to uint64.", value_);
    }
    return static_cast<std::uint64_t>(value_);
  }

  // Bounds:
  static constexpr checked_int min() noexcept { return std::numeric_limits<value_type>::min(); }
  static constexpr checked_int max() noexcept { return std::numeric_limits<value_type>::max(); }

  // Negation.
  constexpr checked_int operator-() const {
    if (value_ == static_cast<value_type>(min())) {
      // Negating will exceed max.
      throw arithmetic_error("Negation of {} produces integer overflow.", value_);
    }
    return -value_;
  }

  // In-place arithmetic operators.
  constexpr checked_int& operator*=(checked_int rhs);
  constexpr checked_int& operator/=(checked_int rhs);
  constexpr checked_int& operator+=(checked_int rhs);
  constexpr checked_int& operator-=(checked_int rhs);

  // Conversion from `unsigned long long` with bounds check.
  static constexpr checked_int from_unsigned_long_long(const unsigned long long int arg) {
    // max < min (in absolute terms), so we only need to check max here
    if (constexpr auto max = static_cast<value_type>(checked_int::max());
        static_cast<unsigned long long int>(max) < arg) {
      throw domain_error("Argument `{}` exceeds checked_int::max.", arg);
    }
    return {static_cast<value_type>(arg)};
  }

  constexpr bool is_even() const noexcept { return !static_cast<bool>(value_ & 1); }

 private:
  value_type value_;
};

// Custom literal suffix.
constexpr checked_int operator"" _chk(unsigned long long int arg) {
  return checked_int::from_unsigned_long_long(arg);
}

// Comparison operators.
constexpr bool operator==(checked_int a, checked_int b) noexcept { return a.value() == b.value(); }
constexpr bool operator!=(checked_int a, checked_int b) noexcept { return a.value() != b.value(); }
constexpr bool operator<(checked_int a, checked_int b) noexcept { return a.value() < b.value(); }
constexpr bool operator>(checked_int a, checked_int b) noexcept { return a.value() > b.value(); }
constexpr bool operator<=(checked_int a, checked_int b) noexcept { return a.value() <= b.value(); }
constexpr bool operator>=(checked_int a, checked_int b) noexcept { return a.value() >= b.value(); }

// Multiplication:
// b > 0, and a * b > MAX or a * b < MIN ---> (a > MAX/b || a < MIN/b)
// b == -1, and a == MIN
// b < -1, and a * b > MAX or a * b < MIN ---> (a < MAX/b || a > MIN/b)
constexpr checked_int operator*(checked_int a, checked_int b) {
  constexpr auto max = checked_int::max().value();
  constexpr auto min = checked_int::min().value();
  if (const auto bv = b.value(); (b > 0 && (a > max / bv || a < min / bv)) ||
                                 (b == -1 && a == min) ||
                                 (b < -1 && (a < max / bv || a > min / bv))) {
    throw arithmetic_error("Multiplication {} * {} produces integer overflow.", a, b);
  }
  return a.value() * b.value();
}

// Division:
constexpr checked_int operator/(checked_int a, checked_int b) {
  if (b == 0) {
    throw arithmetic_error("Encountered division by zero: {} / 0", a);
  } else if (b == -1 && a == checked_int::min()) {
    // Would wrap around to a value greater than > max.
    throw arithmetic_error("Division {} / {} produces integer overflow.", a, b);
  }
  return a.value() / b.value();
}

// Modulo.
// Conditions for modulo are the same as for division: https://stackoverflow.com/questions/19285163
constexpr checked_int operator%(checked_int a, checked_int b) {
  if (b == 0) {
    throw arithmetic_error("Encountered modulo by zero: {} % 0", a);
  } else if (b == -1 && a == checked_int::min()) {
    throw arithmetic_error("Modulo {} % {} produces integer overflow.", a, b);
  }
  return a.value() % b.value();
}

// Addition:
// b > 0, and a + b > MAX ---> a > MAX - b
// b < 0 and a + b < MIN ---> a < MIN - b
constexpr checked_int operator+(checked_int a, checked_int b) {
  constexpr auto max = checked_int::max().value();
  constexpr auto min = checked_int::min().value();
  if ((b > 0 && a > max - b.value()) || (b < 0 && a < min - b.value())) {
    throw arithmetic_error("Addition {} + {} produces integer overflow.", a, b);
  }
  return a.value() + b.value();
}

// Subtraction:
// b > 0, and a - b < MIN ---> a < MIN + b
// b < 0, and a - b > MAX ---> a > MAX + b
constexpr checked_int operator-(checked_int a, checked_int b) {
  constexpr auto max = checked_int::max().value();
  constexpr auto min = checked_int::min().value();
  if ((b > 0 && a < min + b.value()) || (b < 0 && a > max + b.value())) {
    throw arithmetic_error("Subtraction {} - {} produces integer overflow.", a, b);
  }
  return a.value() - b.value();
}

constexpr checked_int& checked_int::operator*=(const checked_int rhs) {
  value_ = static_cast<value_type>(*this * rhs);
  return *this;
}
constexpr checked_int& checked_int::operator/=(const checked_int rhs) {
  value_ = static_cast<value_type>(*this / rhs);
  return *this;
}
constexpr checked_int& checked_int::operator+=(const checked_int rhs) {
  value_ = static_cast<value_type>(*this + rhs);
  return *this;
}
constexpr checked_int& checked_int::operator-=(const checked_int rhs) {
  value_ = static_cast<value_type>(*this - rhs);
  return *this;
}

// Absolute value.
template <typename T, typename = std::enable_if_t<std::is_same_v<checked_int, T>>>
constexpr checked_int abs(const T x) {
  if (x < 0) {
    return -x;
  }
  return x;
}

// Greatest common divisor via std::gcd.
// Throw if conversion from negative to positive would exceed range of numeric type.
constexpr checked_int gcd(const checked_int a, const checked_int b) {
  return std::gcd(abs(a).value(), abs(b).value());
}

// Hashing of integers.
template <>
struct hash_struct<checked_int> {
  std::size_t operator()(const checked_int value) const noexcept {
    // Don't have bit_cast, so memcpy into size_t to avoid UB.
    static_assert(std::is_trivially_copyable_v<checked_int> &&
                  sizeof(checked_int) == sizeof(std::size_t));
    std::size_t hash;
    std::memcpy(static_cast<void*>(&hash), static_cast<const void*>(&value), sizeof(value));
    return hash;
  }
};

}  // namespace wf

// Support formatting of `checked_int`.
template <>
struct fmt::formatter<wf::checked_int, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::checked_int& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.value());
  }
};
