// Copyright 2022 Gareth Cross
#pragma once
#include <cmath>
#include <numeric>
#include <optional>

#include "wf/assertions.h"
#include "wf/expression_concept.h"
#include "wf/expression_impl.h"
#include "wf/hashing.h"

namespace math {

class Integer;
class Float;
class Rational;

// An integral constant.
class Integer {
 public:
  static constexpr std::string_view name_str = "Integer";
  static constexpr bool is_leaf_node = true;

  using IntegralType = int64_t;

  // Construct from number.
  explicit constexpr Integer(IntegralType val) noexcept : val_(val) {}

  // Check if numerical constants are completely identical.
  constexpr bool is_identical_to(const Integer& other) const noexcept { return val_ == other.val_; }

  // Access numeric value.
  constexpr IntegralType get_value() const noexcept { return val_; }

  // True if this integer is zero.
  constexpr bool is_zero() const noexcept { return val_ == static_cast<IntegralType>(0); }

  // True if this integer is greater than zero.
  constexpr bool is_positive() const noexcept { return val_ > static_cast<IntegralType>(0); }

  // True if this integer is less than zero.
  constexpr bool is_negative() const noexcept { return val_ < static_cast<IntegralType>(0); }

  // True if this integer is even. (Zero is even).
  constexpr bool is_even() const noexcept { return !static_cast<bool>(val_ & 1); }

  // Cast to integer:
  constexpr explicit operator Float() const;

  // Negate:
  Integer operator-() const { return Integer{-val_}; }

  // Get absolute value.
  Integer abs() const { return Integer{std::abs(val_)}; }

  // Create an integer expression.
  static Expr create(IntegralType x);
  static Expr create(const Integer& x) { return create(x.get_value()); }

 private:
  IntegralType val_;
};

// A rational value (in form numerator / denominator).
class Rational {
 public:
  static constexpr std::string_view name_str = "Rational";
  static constexpr bool is_leaf_node = true;

  using IntegralType = Integer::IntegralType;

  // Construct a rational. Conversion to canonical form is automatic.
  constexpr Rational(IntegralType n, IntegralType d) : Rational(create_pair(n, d)) {}

  // Construct a rational from an integer value.
  explicit constexpr Rational(const Integer& integer) : n_(integer.get_value()), d_(1) {}

  constexpr bool is_identical_to(const Rational& other) const noexcept {
    return n_ == other.n_ && d_ == other.d_;
  }

  // Access numerator and denominator.
  constexpr IntegralType numerator() const noexcept { return n_; }
  constexpr IntegralType denominator() const noexcept { return d_; }

  // Cast to float.
  explicit operator Float() const;

  // True if numerator equals denominator.
  constexpr bool is_one() const noexcept { return n_ == d_; }

  // True if numerator is zero.
  constexpr bool is_zero() const noexcept { return n_ == static_cast<IntegralType>(0); }

  // True if positive (numerator is > 0).
  constexpr bool is_positive() const noexcept { return n_ > static_cast<IntegralType>(0); }

  // True if negative (only the numerator may be < 0).
  constexpr bool is_negative() const noexcept { return n_ < static_cast<IntegralType>(0); }

  // Try converting the rational to an integer. If the numerator and denominator divide
  // evenly, returns a valid optional.
  constexpr std::optional<Integer> try_convert_to_integer() const noexcept {
    if (n_ % d_ == 0) {
      return {Integer(n_ / d_)};
    }
    return {};
  }

  // Normalize a rational into a whole integer part, and a rational whose absolute value is less
  // than one.
  constexpr std::pair<Integer, Rational> normalized() const {
    return std::make_pair(Integer{n_ / d_}, Rational{n_ % d_, d_});
  }

  // True if the absolute value of the fraction is less than one.
  bool is_proper() const noexcept { return std::abs(n_) < d_; }

  // Create a rational expression and simplify if possible.
  static Expr create(Rational r) {
    if (auto as_int = r.try_convert_to_integer(); as_int) {
      return Integer::create(as_int->get_value());
    }
    return make_expr<Rational>(r);
  }

  // Create a rational expression and simplify if possible.
  static Expr create(IntegralType n, IntegralType d) { return create(Rational{n, d}); }

 private:
  constexpr std::pair<IntegralType, IntegralType> create_pair(IntegralType n, IntegralType d) {
    // Find the largest common denominator and reduce:
    const IntegralType gcd = std::gcd(n, d);
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
  explicit constexpr Rational(std::pair<IntegralType, IntegralType> pair)
      : n_(pair.first), d_(pair.second) {}

  IntegralType n_;
  IntegralType d_;
};

// A floating point constant.
class Float {
 public:
  static constexpr std::string_view name_str = "Float";
  static constexpr bool is_leaf_node = true;

  using FloatType = double;

  // Construct from float value.
  explicit constexpr Float(FloatType val) noexcept : val_(val) {}

  // Check if numerical constants are completely identical.
  constexpr bool is_identical_to(const Float& other) const noexcept { return val_ == other.val_; }

  // Access numeric value.
  constexpr FloatType get_value() const noexcept { return val_; }

  // Is this float identical to zero?
  constexpr bool is_zero() const noexcept { return val_ == static_cast<FloatType>(0); }

  // True if the float is negative.
  constexpr bool is_positive() const noexcept { return val_ > static_cast<FloatType>(0); }

  // True if the float is negative.
  constexpr bool is_negative() const noexcept { return val_ < static_cast<FloatType>(0); }

  // Get absolute value.
  Float abs() const { return Float{std::abs(val_)}; }

  // Create floating point expression.
  static Expr create(Float f) { return make_expr<Float>(f); }
  static Expr create(FloatType f) {
    WF_ASSERT(std::isfinite(f), "Float values must be finite: {}", f);
    return create(Float{f});
  }

 private:
  FloatType val_;
};

// Operations on integers:
inline constexpr auto operator*(const Integer& a, const Integer& b) {
  return Integer{a.get_value() * b.get_value()};
}
inline constexpr auto operator+(const Integer& a, const Integer& b) {
  return Integer{a.get_value() + b.get_value()};
}
inline constexpr bool operator<(const Integer& a, const Integer& b) {
  return a.get_value() < b.get_value();
}
inline constexpr bool operator==(const Integer& a, const Integer& b) {
  return a.get_value() == b.get_value();
}

inline constexpr Integer::operator Float() const {
  return Float{static_cast<Float::FloatType>(val_)};
}

// Hashing of integers. Like std::hash, just pass the value through.
template <>
struct hash_struct<Integer::IntegralType> {
  std::size_t operator()(Integer::IntegralType value) const {
    return std::hash<Integer::IntegralType>{}(value);
  }
};
template <>
struct hash_struct<Integer> {
  std::size_t operator()(const Integer& value) const {
    return hash_struct<Integer::IntegralType>{}(value.get_value());
  }
};

// Operations on rationals:
inline constexpr auto operator*(const Rational& a, const Rational& b) {
  return Rational{a.numerator() * b.numerator(), a.denominator() * b.denominator()};
}
inline constexpr auto operator/(const Rational& a, const Rational& b) {
  return Rational{a.numerator() * b.denominator(), a.denominator() * b.numerator()};
}
inline constexpr auto operator+(const Rational& a, const Rational& b) {
  // Create common denominator and create a new rational:
  return Rational{a.numerator() * b.denominator() + b.numerator() * a.denominator(),
                  a.denominator() * b.denominator()};
}
inline constexpr auto operator-(const Rational& a, const Rational& b) {
  return Rational{a.numerator() * b.denominator() - b.numerator() * a.denominator(),
                  a.denominator() * b.denominator()};
}
inline constexpr auto operator%(const Rational& a, const Rational& b) {
  // Divide a/b, then determine the remainder after dropping the integer part.
  const Rational quotient = a / b;
  return Rational{quotient.numerator() % quotient.denominator(), quotient.denominator()};
}

inline constexpr bool operator<(const Rational& a, const Rational& b) {
  // TODO: Watch for overflow.
  return a.numerator() * b.denominator() < b.numerator() * a.denominator();
}
inline constexpr bool operator>(const Rational& a, const Rational& b) {
  return a.numerator() * b.denominator() > b.numerator() * a.denominator();
}
inline constexpr bool operator==(const Rational& a, const Rational& b) {
  // Constructor ensures we reduce to common denominator, so we can compare directly.
  return a.numerator() == b.numerator() && a.denominator() == b.denominator();
}
inline constexpr bool operator!=(const Rational& a, const Rational& b) { return !operator==(a, b); }

inline Rational::operator Float() const {
  // TODO: Look up if there is a more accurate way of doing this.
  return Float{static_cast<Float::FloatType>(n_) / static_cast<Float::FloatType>(d_)};
}

// Hashing of rationals.
template <>
struct hash_struct<Rational> {
  std::size_t operator()(const Rational& r) const {
    return hash_args(0, r.numerator(), r.denominator());
  }
};

// Wrap an angle specified as a rational multiple of pi into the range (-pi, pi]. A new rational
// coefficient between (-1, 1] is returned.
inline constexpr Rational mod_pi_rational(const Rational& r) {
  // Split into integer and rational parts:
  const auto [integer_part_unwrapped, fractional_part] = r.normalized();
  // Wrap the integer part into (-2, 2).
  const int64_t integer_part = integer_part_unwrapped.get_value() % 2;
  // Now we want to convert into range (-1, 1]:
  if (integer_part == 1) {
    return fractional_part.is_zero() ? Rational{1, 1} : fractional_part - Rational{1, 1};
  } else if (integer_part == -1) {
    return Rational{1, 1} + fractional_part;
  }
  return fractional_part;
}

// Operations on floats:
inline constexpr auto operator*(const Float& a, const Float& b) {
  return Float{a.get_value() * b.get_value()};
}
inline constexpr auto operator+(const Float& a, const Float& b) {
  return Float{a.get_value() + b.get_value()};
}
inline constexpr bool operator<(const Float& a, const Float& b) {
  return a.get_value() < b.get_value();
}
inline constexpr bool operator==(const Float& a, const Float& b) {
  return a.get_value() == b.get_value();
}
inline constexpr bool operator!=(const Float& a, const Float& b) {
  return a.get_value() != b.get_value();
}

// Hashing of floats.
template <>
struct hash_struct<Float> {
  // Can't be constexpr, because std::hash is not constexpr.
  std::size_t operator()(const Float& f) const {
    return std::hash<Float::FloatType>{}(f.get_value());
  }
};

// Will evaluate to true if A or B (or both) is a float, w/ the other being Integer or Rational.
// This is so we can promote integers/rationals -> float when they are combined with floats.
template <typename A, typename B>
constexpr bool is_float_and_numeric_v =
    (std::is_same_v<A, Float> && type_list_contains_type_v<B, Integer, Rational>) ||
    (std::is_same_v<B, Float> && type_list_contains_type_v<A, Integer, Rational>) ||
    (std::is_same_v<A, Float> && std::is_same_v<B, Float>);

static_assert(is_float_and_numeric_v<Float, Float>);
static_assert(is_float_and_numeric_v<Float, Integer>);
static_assert(is_float_and_numeric_v<Rational, Float>);
static_assert(!is_float_and_numeric_v<Integer, Integer>);
static_assert(!is_float_and_numeric_v<Integer, Rational>);

}  // namespace math

// Formatters
template <>
struct fmt::formatter<math::Integer, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Integer& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.get_value());
  }
};

template <>
struct fmt::formatter<math::Rational, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Rational& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "({} / {})", x.numerator(), x.denominator());
  }
};

template <>
struct fmt::formatter<math::Float, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const math::Float& x, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", x.get_value());
  }
};
