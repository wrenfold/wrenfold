// Copyright 2022 Gareth Cross
#pragma once
#include <cmath>
#include <numeric>
#include <optional>

#include "wf/assertions.h"
#include "wf/hashing.h"
#include "wf/type_list.h"

namespace wf {

class integer_constant;
class float_constant;
class rational_constant;

// An integral constant.
class integer_constant {
 public:
  static constexpr std::string_view name_str = "Integer";
  static constexpr bool is_leaf_node = true;

  using value_type = std::int64_t;

  // Construct from number.
  explicit constexpr integer_constant(value_type val) noexcept : val_(val) {}

  // Check if numerical constants are completely identical.
  constexpr bool is_identical_to(const integer_constant& other) const noexcept {
    return val_ == other.val_;
  }

  // Access numeric value.
  constexpr value_type get_value() const noexcept { return val_; }

  // True if this integer is zero.
  constexpr bool is_zero() const noexcept { return val_ == static_cast<value_type>(0); }

  // True if this integer is greater than zero.
  constexpr bool is_positive() const noexcept { return val_ > static_cast<value_type>(0); }

  // True if this integer is less than zero.
  constexpr bool is_negative() const noexcept { return val_ < static_cast<value_type>(0); }

  // True if this integer is even. (Zero is even).
  constexpr bool is_even() const noexcept { return !static_cast<bool>(val_ & 1); }

  // Cast to integer:
  constexpr explicit operator float_constant() const;

  // Negate:
  integer_constant operator-() const { return integer_constant{-val_}; }

  // Get absolute value.
  integer_constant abs() const { return integer_constant{std::abs(val_)}; }

 private:
  value_type val_;
};

// A rational value (in form numerator / denominator).
class rational_constant {
 public:
  static constexpr std::string_view name_str = "Rational";
  static constexpr bool is_leaf_node = true;

  using value_type = integer_constant::value_type;

  // Construct a rational. Conversion to canonical form is automatic.
  constexpr rational_constant(value_type n, value_type d) : rational_constant(create_pair(n, d)) {}

  // Construct a rational from an integer value.
  explicit constexpr rational_constant(const integer_constant& integer) noexcept
      : n_(integer.get_value()), d_(1) {}

  constexpr bool is_identical_to(const rational_constant& other) const noexcept {
    return n_ == other.n_ && d_ == other.d_;
  }

  // Access numerator and denominator.
  constexpr value_type numerator() const noexcept { return n_; }
  constexpr value_type denominator() const noexcept { return d_; }

  // Cast to float.
  explicit operator float_constant() const;

  // True if numerator equals denominator.
  constexpr bool is_one() const noexcept { return n_ == d_; }

  // True if numerator is zero.
  constexpr bool is_zero() const noexcept { return n_ == static_cast<value_type>(0); }

  // True if positive (numerator is > 0).
  constexpr bool is_positive() const noexcept { return n_ > static_cast<value_type>(0); }

  // True if negative (only the numerator may be < 0).
  constexpr bool is_negative() const noexcept { return n_ < static_cast<value_type>(0); }

  // Try converting the rational to an integer. If the numerator and denominator divide
  // evenly, returns a valid optional.
  constexpr std::optional<integer_constant> try_convert_to_integer() const noexcept {
    if (n_ % d_ == 0) {
      return {integer_constant(n_ / d_)};
    }
    return {};
  }

  // Normalize a rational into a whole integer part, and a rational whose absolute value is less
  // than one.
  constexpr std::pair<integer_constant, rational_constant> normalized() const noexcept {
    return std::make_pair(integer_constant{n_ / d_}, rational_constant{n_ % d_, d_});
  }

  // True if the absolute value of the fraction is less than one.
  bool is_proper() const noexcept { return std::abs(n_) < d_; }

 private:
  constexpr std::pair<value_type, value_type> create_pair(value_type n, value_type d) noexcept {
    // Find the largest common denominator and reduce:
    const value_type gcd = std::gcd(n, d);
    n = n / gcd;
    d = d / gcd;
    if (d < 0) {
      // If denominator is < 0, transfer sign to numerator.
      // If both are negative, this will make both positive.
      return std::make_pair(-n, -d);
    } else {
      return std::make_pair(n, d);
    }
  }

  // Private constructor to allow direct initialization.
  explicit constexpr rational_constant(std::pair<value_type, value_type> pair)
      : n_(pair.first), d_(pair.second) {}

  value_type n_;
  value_type d_;
};

// A floating point constant.
class float_constant {
 public:
  static constexpr std::string_view name_str = "Float";
  static constexpr bool is_leaf_node = true;

  using value_type = double;

  // Construct from float value.
  explicit constexpr float_constant(value_type val) noexcept : val_(val) {}

  // Check if numerical constants are completely identical.
  constexpr bool is_identical_to(const float_constant& other) const noexcept {
    return val_ == other.val_;
  }

  // Access numeric value.
  constexpr value_type get_value() const noexcept { return val_; }

  // Is this float identical to zero?
  constexpr bool is_zero() const noexcept { return val_ == static_cast<value_type>(0); }

  // True if the float is negative.
  constexpr bool is_positive() const noexcept { return val_ > static_cast<value_type>(0); }

  // True if the float is negative.
  constexpr bool is_negative() const noexcept { return val_ < static_cast<value_type>(0); }

  // Get absolute value.
  float_constant abs() const noexcept { return float_constant{std::abs(val_)}; }

 private:
  value_type val_;
};

// Operations on integers:
inline constexpr auto operator*(const integer_constant& a, const integer_constant& b) {
  return integer_constant{a.get_value() * b.get_value()};
}
inline constexpr auto operator+(const integer_constant& a, const integer_constant& b) {
  return integer_constant{a.get_value() + b.get_value()};
}
inline constexpr bool operator<(const integer_constant& a, const integer_constant& b) {
  return a.get_value() < b.get_value();
}
inline constexpr bool operator==(const integer_constant& a, const integer_constant& b) {
  return a.get_value() == b.get_value();
}

inline constexpr integer_constant::operator float_constant() const {
  return float_constant{static_cast<float_constant::value_type>(val_)};
}

// Hashing of integers.
template <>
struct hash_struct<integer_constant::value_type> {
  std::size_t operator()(const integer_constant::value_type value) const noexcept {
    // Don't have bit_cast, so memcpy into size_t to avoid UB.
    static_assert(sizeof(integer_constant::value_type) == sizeof(std::size_t));
    std::size_t hash;
    std::memcpy(&hash, static_cast<const void*>(&value), sizeof(value));
    return hash;
  }
};

template <>
struct hash_struct<integer_constant> {
  std::size_t operator()(const integer_constant value) const noexcept {
    return hash(value.get_value());
  }
};

template <>
struct is_identical_struct<integer_constant> {
  constexpr bool operator()(const integer_constant a, const integer_constant b) const noexcept {
    return a.get_value() == b.get_value();
  }
};

// Operations on rationals:
inline constexpr auto operator*(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.numerator(), a.denominator() * b.denominator()};
}
inline constexpr auto operator/(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.denominator(), a.denominator() * b.numerator()};
}
inline constexpr auto operator+(const rational_constant& a, const rational_constant& b) {
  // Create common denominator and create a new rational:
  return rational_constant{a.numerator() * b.denominator() + b.numerator() * a.denominator(),
                           a.denominator() * b.denominator()};
}
inline constexpr auto operator-(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.denominator() - b.numerator() * a.denominator(),
                           a.denominator() * b.denominator()};
}
inline constexpr auto operator%(const rational_constant& a, const rational_constant& b) {
  // Divide a/b, then determine the remainder after dropping the integer part.
  const rational_constant quotient = a / b;
  return rational_constant{quotient.numerator() % quotient.denominator(), quotient.denominator()};
}

inline constexpr bool operator<(const rational_constant& a, const rational_constant& b) {
  // TODO: Watch for overflow.
  return a.numerator() * b.denominator() < b.numerator() * a.denominator();
}
inline constexpr bool operator>(const rational_constant& a, const rational_constant& b) {
  return a.numerator() * b.denominator() > b.numerator() * a.denominator();
}
inline constexpr bool operator==(const rational_constant& a, const rational_constant& b) {
  // Constructor ensures we reduce to common denominator, so we can compare directly.
  return a.numerator() == b.numerator() && a.denominator() == b.denominator();
}
inline constexpr bool operator!=(const rational_constant& a, const rational_constant& b) {
  return !operator==(a, b);
}

inline rational_constant::operator float_constant() const {
  // TODO: Look up if there is a more accurate way of doing this.
  return float_constant{static_cast<float_constant::value_type>(n_) /
                        static_cast<float_constant::value_type>(d_)};
}

// Hashing of rationals.
template <>
struct hash_struct<rational_constant> {
  std::size_t operator()(const rational_constant& r) const {
    return hash_combine(hash(r.numerator()), hash(r.denominator()));
  }
};

template <>
struct is_identical_struct<rational_constant> {
  constexpr bool operator()(const rational_constant& a, const rational_constant& b) const noexcept {
    return a.numerator() == b.numerator() && a.denominator() == b.denominator();
  }
};

// Wrap an angle specified as a rational multiple of pi into the range (-pi, pi]. A new rational
// coefficient between (-1, 1] is returned.
inline constexpr rational_constant mod_pi_rational(const rational_constant& r) noexcept {
  // Split into integer and rational parts:
  const auto [integer_part_unwrapped, fractional_part] = r.normalized();
  // Wrap the integer part into (-2, 2), then convert into range (-1, 1]:
  if (const int64_t integer_part = integer_part_unwrapped.get_value() % 2; integer_part == 1) {
    return fractional_part.is_zero() ? rational_constant{1, 1}
                                     : fractional_part - rational_constant{1, 1};
  } else if (integer_part == -1) {
    return rational_constant{1, 1} + fractional_part;
  }
  return fractional_part;
}

// Operations on floats:
inline constexpr auto operator*(const float_constant& a, const float_constant& b) {
  return float_constant{a.get_value() * b.get_value()};
}
inline constexpr auto operator+(const float_constant& a, const float_constant& b) {
  return float_constant{a.get_value() + b.get_value()};
}
inline constexpr bool operator<(const float_constant& a, const float_constant& b) {
  return a.get_value() < b.get_value();
}
inline constexpr bool operator==(const float_constant& a, const float_constant& b) {
  return a.get_value() == b.get_value();
}
inline constexpr bool operator!=(const float_constant& a, const float_constant& b) {
  return a.get_value() != b.get_value();
}

// Hashing of floats.
template <>
struct hash_struct<float_constant> {
  std::size_t operator()(const float_constant& f) const {
    static_assert(sizeof(float_constant::value_type) == sizeof(std::size_t));
    std::size_t hash;
    const auto value = f.get_value();
    std::memcpy(&hash, static_cast<const void*>(&value), sizeof(value));
    return hash;
  }
};

template <>
struct is_identical_struct<float_constant> {
  constexpr bool operator()(const float_constant a, const float_constant b) const noexcept {
    return a.get_value() == b.get_value();
  }
};

// Will evaluate to true if A or B (or both) is a float, w/ the other being Integer or Rational.
// This is so we can promote integers/rationals -> float when they are combined with floats.
template <typename A, typename B>
constexpr bool is_float_and_numeric_v =
    (std::is_same_v<A, float_constant> &&
     type_list_contains_v<B, integer_constant, rational_constant>) ||
    (std::is_same_v<B, float_constant> &&
     type_list_contains_v<A, integer_constant, rational_constant>) ||
    (std::is_same_v<A, float_constant> && std::is_same_v<B, float_constant>);

static_assert(is_float_and_numeric_v<float_constant, float_constant>);
static_assert(is_float_and_numeric_v<float_constant, integer_constant>);
static_assert(is_float_and_numeric_v<rational_constant, float_constant>);
static_assert(!is_float_and_numeric_v<integer_constant, integer_constant>);
static_assert(!is_float_and_numeric_v<integer_constant, rational_constant>);

}  // namespace wf

// Formatters
template <>
struct fmt::formatter<wf::integer_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::integer_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.get_value());
  }
};

template <>
struct fmt::formatter<wf::rational_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::rational_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "({} / {})", x.numerator(), x.denominator());
  }
};

template <>
struct fmt::formatter<wf::float_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::float_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.get_value());
  }
};
