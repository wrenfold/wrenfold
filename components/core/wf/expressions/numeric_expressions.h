// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <cmath>
#include <numeric>
#include <optional>

#include "wf/utility/assertions.h"
#include "wf/utility/checked_int.h"
#include "wf/utility/hashing.h"
#include "wf/utility/ordering.h"

namespace wf {

class integer_constant;
class float_constant;
class rational_constant;

// An integral constant.
class integer_constant {
 public:
  static constexpr std::string_view name_str = "IntegerConstant";
  static constexpr bool is_leaf_node = true;

  using value_type = checked_int;

  // Construct from number.
  explicit constexpr integer_constant(const value_type val) noexcept : val_(val) {}

  // Access numeric value.
  constexpr value_type value() const noexcept { return val_; }

  // True if this integer is zero.
  constexpr bool is_zero() const noexcept { return val_ == 0; }

  // True if the integer is one.
  constexpr bool is_one() const noexcept { return val_ == 1; }

  // True if this integer is greater than zero.
  constexpr bool is_positive() const noexcept { return val_ > 0; }

  // True if this integer is less than zero.
  constexpr bool is_negative() const noexcept { return val_ < 0; }

  // True if this integer is even. (Zero is even).
  constexpr bool is_even() const noexcept { return val_.is_even(); }

  // Cast to integer:
  constexpr explicit operator float_constant() const;

  // Negate:
  integer_constant operator-() const { return integer_constant{-val_}; }

  // Get absolute value.
  integer_constant abs() const { return integer_constant{wf::abs(val_)}; }

 private:
  value_type val_;
};

// A rational value (in form numerator / denominator).
class rational_constant {
 public:
  static constexpr std::string_view name_str = "RationalConstant";
  static constexpr bool is_leaf_node = true;

  using value_type = checked_int;

  // Construct a rational. Conversion to canonical form is automatic.
  constexpr rational_constant(const value_type n, const value_type d)
      : rational_constant(create_pair(n, d)) {}

  // Construct a rational from an integer value.
  explicit constexpr rational_constant(const integer_constant integer) noexcept
      : n_(integer.value()), d_(1) {}

  // Access numerator and denominator.
  constexpr value_type numerator() const noexcept { return n_; }
  constexpr value_type denominator() const noexcept { return d_; }

  // Cast to float.
  explicit operator float_constant() const;

  // True if numerator equals denominator.
  constexpr bool is_one() const noexcept { return n_ == d_; }

  // True if numerator is zero.
  constexpr bool is_zero() const noexcept { return n_ == 0; }

  // True if positive (numerator is > 0).
  constexpr bool is_positive() const noexcept { return n_ > 0; }

  // True if negative (only the numerator may be < 0).
  constexpr bool is_negative() const noexcept { return n_ < 0; }

  // True if the rational is actually an integer in disguise.
  constexpr bool is_integer() const noexcept { return n_ % d_ == 0; }

  // Try converting the rational to an integer. If the numerator and denominator divide
  // evenly, returns a valid optional.
  constexpr std::optional<integer_constant> try_convert_to_integer() const noexcept {
    if (is_integer()) {
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
  bool is_proper() const noexcept { return -d_ < n_ && n_ < d_; }

 private:
  static constexpr std::pair<value_type, value_type> create_pair(value_type n, value_type d) {
    if (d == 0) {
      throw arithmetic_error("Rational denominator must be non-zero: {} / {}", n, d);
    }
    // Signed std::gcd requires representable magnitudes, which excludes INT64_MIN.
    const auto magnitude = [](const value_type value) constexpr {
      const auto bits = static_cast<std::uint64_t>(value.value());
      return value < 0 ? 0 - bits : bits;
    };
    const auto unsigned_gcd = std::gcd(magnitude(n), magnitude(d));
    if (unsigned_gcd > static_cast<std::uint64_t>(checked_int::max().value())) {
      // With d != 0, a gcd of 2^63 occurs only for 0/INT64_MIN or INT64_MIN/INT64_MIN.
      return {n == 0 ? 0 : 1, 1};
    }
    const auto gcd = static_cast<std::int64_t>(unsigned_gcd);
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
  explicit constexpr rational_constant(const std::pair<value_type, value_type>& pair) noexcept
      : n_(pair.first), d_(pair.second) {}

  value_type n_;
  value_type d_;
};

// A floating point constant.
class float_constant {
 public:
  static constexpr std::string_view name_str = "FloatConstant";
  static constexpr bool is_leaf_node = true;

  using value_type = double;

  // Construct from float value.
  explicit constexpr float_constant(const value_type val) noexcept : val_(val) {}

  // Access numeric value.
  constexpr value_type value() const noexcept { return val_; }

  // Is this float identical to zero?
  constexpr bool is_zero() const noexcept { return val_ == static_cast<value_type>(0); }

  // True if the float is negative.
  constexpr bool is_positive() const noexcept { return val_ > static_cast<value_type>(0); }

  // True if the float is negative.
  constexpr bool is_negative() const noexcept { return val_ < static_cast<value_type>(0); }

  // Get absolute value.
  float_constant abs() const noexcept { return float_constant{std::abs(val_)}; }

  // True if the underlying float is NaN.
  bool is_nan() const noexcept { return std::isnan(val_); }

 private:
  value_type val_;
};

// Operations on integers:
constexpr bool operator<(const integer_constant& a, const integer_constant& b) noexcept {
  return a.value() < b.value();
}
constexpr bool operator==(const integer_constant& a, const integer_constant& b) noexcept {
  return a.value() == b.value();
}
constexpr bool operator!=(const integer_constant& a, const integer_constant& b) noexcept {
  return a.value() != b.value();
}

constexpr integer_constant::operator float_constant() const {
  return float_constant{static_cast<float_constant::value_type>(val_)};
}

// Hashing of integers.
template <>
struct hash_struct<integer_constant> {
  std::size_t operator()(const integer_constant value) const noexcept {
    return hash(value.value());
  }
};

template <>
struct is_identical_struct<integer_constant> {
  constexpr bool operator()(const integer_constant a, const integer_constant b) const noexcept {
    return a.value() == b.value();
  }
};

template <>
struct order_struct<integer_constant> {
  constexpr relative_order operator()(const integer_constant a,
                                      const integer_constant b) const noexcept {
    return order_by_comparison(a, b);
  }
};

// Operations on rationals:
constexpr auto operator*(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.numerator(), a.denominator() * b.denominator()};
}
constexpr auto operator*(const rational_constant& a, const checked_int b) {
  return rational_constant{a.numerator() * b, a.denominator()};
}
constexpr auto operator/(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.denominator(), a.denominator() * b.numerator()};
}

constexpr auto operator+(const rational_constant& a, const rational_constant& b) {
  // Create common denominator and create a new rational:
  return rational_constant{a.numerator() * b.denominator() + b.numerator() * a.denominator(),
                           a.denominator() * b.denominator()};
}

constexpr auto operator-(const rational_constant& a, const rational_constant& b) {
  return rational_constant{a.numerator() * b.denominator() - b.numerator() * a.denominator(),
                           a.denominator() * b.denominator()};
}

constexpr auto operator%(const rational_constant& a, const rational_constant& b) {
  // Divide a/b, then determine the remainder after dropping the integer part.
  const rational_constant quotient = a / b;
  return rational_constant{quotient.numerator() % quotient.denominator(), quotient.denominator()};
}

template <>
struct order_struct<rational_constant> {
  constexpr relative_order operator()(const rational_constant& a,
                                      const rational_constant& b) const noexcept {
    if (a.denominator() == b.denominator()) {
      return order_by_comparison(a.numerator(), b.numerator());
    }
    if (a.is_negative() != b.is_negative()) {
      return order_by_comparison(a.numerator(), b.numerator());
    }

    // Compare magnitudes, reversing the result for two negative fractions. Unsigned
    // arithmetic handles the magnitude of INT64_MIN without overflowing negation.
    auto left_n = static_cast<std::uint64_t>(a.numerator().value());
    auto right_n = static_cast<std::uint64_t>(b.numerator().value());
    if (a.is_negative()) {
      left_n = 0 - left_n;
      right_n = 0 - right_n;
    }
    auto left_d = static_cast<std::uint64_t>(a.denominator().value());
    auto right_d = static_cast<std::uint64_t>(b.denominator().value());
    bool reverse = a.is_negative();
    while (true) {
      const auto left_q = left_n / left_d;
      const auto right_q = right_n / right_d;
      if (left_q != right_q) {
        return reverse ? order_by_comparison(right_q, left_q)
                       : order_by_comparison(left_q, right_q);
      }
      const auto left_r = left_n % left_d;
      const auto right_r = right_n % right_d;
      if (left_r == 0 || right_r == 0) {
        return reverse ? order_by_comparison(right_r, left_r)
                       : order_by_comparison(left_r, right_r);
      }
      // Equal integer parts leave only the fractional remainders. Compare their
      // reciprocals, which reverses the order and reduces the denominators.
      left_n = left_d;
      left_d = left_r;
      right_n = right_d;
      right_d = right_r;
      reverse = !reverse;
    }
  }
};

constexpr bool operator<(const rational_constant& a, const rational_constant& b) noexcept {
  return order_struct<rational_constant>{}(a, b) == relative_order::less_than;
}

constexpr bool operator>(const rational_constant& a, const rational_constant& b) noexcept {
  return order_struct<rational_constant>{}(a, b) == relative_order::greater_than;
}

constexpr bool operator==(const rational_constant& a, const rational_constant& b) noexcept {
  // Constructor ensures we reduce to common denominator, so we can compare directly.
  return a.numerator() == b.numerator() && a.denominator() == b.denominator();
}

constexpr bool operator!=(const rational_constant& a, const rational_constant& b) noexcept {
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
  std::size_t operator()(const rational_constant& r) const noexcept {
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
constexpr rational_constant mod_pi_rational(const rational_constant& r) {
  // Split into integer and rational parts:
  const auto [integer_part_unwrapped, fractional_part] = r.normalized();
  // Wrap the integer part into (-2, 2), then convert into range (-1, 1]:
  if (const checked_int integer_part = integer_part_unwrapped.value() % 2; integer_part == 1) {
    return fractional_part.is_zero() ? rational_constant{1, 1}
                                     : fractional_part - rational_constant{1, 1};
  } else if (integer_part == -1) {
    return rational_constant{1, 1} + fractional_part;
  }
  return fractional_part;
}

// Operations on floats:
constexpr auto operator*(const float_constant& a, const float_constant& b) noexcept {
  return float_constant{a.value() * b.value()};
}

constexpr auto operator+(const float_constant& a, const float_constant& b) noexcept {
  return float_constant{a.value() + b.value()};
}

constexpr bool operator<(const float_constant& a, const float_constant& b) noexcept {
  return a.value() < b.value();
}

constexpr bool operator==(const float_constant& a, const float_constant& b) noexcept {
  return a.value() == b.value();
}

constexpr bool operator!=(const float_constant& a, const float_constant& b) noexcept {
  return a.value() != b.value();
}

// Hashing of floats.
template <>
struct hash_struct<float_constant> {
  std::size_t operator()(const float_constant& f) const noexcept {
    if (f.is_zero()) {
      return 0;
    }
    static_assert(std::is_trivially_copyable_v<float_constant> &&
                  sizeof(float_constant) == sizeof(std::size_t));
    std::size_t hash;
    std::memcpy(&hash, static_cast<const void*>(&f), sizeof(f));
    return hash;
  }
};

template <>
struct is_identical_struct<float_constant> {
  constexpr bool operator()(const float_constant a, const float_constant b) const noexcept {
    return a.value() == b.value();
  }
};

template <>
struct order_struct<float_constant> {
  relative_order operator()(const float_constant& a, const float_constant& b) const {
    return order_by_comparison(a, b);
  }
};

}  // namespace wf

// Formatters
template <>
struct fmt::formatter<wf::integer_constant, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const wf::integer_constant& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.value());
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
    return fmt::format_to(ctx.out(), "{}", x.value());
  }
};
