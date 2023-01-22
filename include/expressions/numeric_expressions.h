// Copyright 2022 Gareth Cross
#pragma once
#include <cmath>
#include <numeric>
#include <optional>

#include "assertions.h"
#include "constants.h"
#include "expression_concept.h"
#include "expression_impl.h"

namespace math {

class Integer;
class Float;
class Rational;

// An integral constant.
class Integer : public ExpressionImpl<Integer> {
 public:
  using IntegralType = int64_t;

  // Construct from number.
  explicit Integer(IntegralType val) : val_(val) {}

  // Check if numerical constants are completely identical.
  bool IsIdenticalToImplTyped(const Integer& other) const { return val_ == other.val_; }

  // Access numeric value.
  IntegralType GetValue() const { return val_; }

  // Cast to integer:
  explicit operator Float() const;

  // Cast to rational:
  explicit operator Rational() const;

  // Negate:
  Integer operator-() const { return Integer{-val_}; }

  // Get absolute value.
  Integer Abs() const { return Integer{std::abs(val_)}; }

  // Create an integer expression.
  static Expr Create(IntegralType x);
  static Expr Create(const Integer& x) { return Create(x.GetValue()); }

 private:
  IntegralType val_;
};

// A rational value (in form numerator / denominator).
class Rational : public ExpressionImpl<Rational> {
 public:
  using IntegralType = Integer::IntegralType;

  // Construct a rational. Conversion to canonical form is automatic.
  Rational(IntegralType numerator, IntegralType denominator) {
    // Find the largest common denominator and reduce:
    const IntegralType gcd = std::gcd(numerator, denominator);
    n_ = numerator / gcd;
    d_ = denominator / gcd;
    if (d_ < 0) {
      // If denominator is < 0, transfer sign to numerator.
      // If both are negative, this will make both positive.
      n_ = -n_;
      d_ = -d_;
    }
    ASSERT_GREATER(d_, 0, "Cannot construct rational w/ zero denominator.");
  }

  bool IsIdenticalToImplTyped(const Rational& other) const {
    return n_ == other.n_ && d_ == other.d_;
  }

  // Access numerator and denominator.
  IntegralType Numerator() const { return n_; }
  IntegralType Denominator() const { return d_; }

  // Cast to float.
  explicit operator Float() const;

  // True if numerator equals denominator.
  bool IsOne() const { return n_ == d_; }

  // True if numerator is zero.
  bool IsZero() const { return n_ == 0; }

  // Try converting the rational to an integer. If the numerator and denominator divide
  // evenly, returns a valid optional.
  std::optional<Integer> TryConvertToInteger() const {
    if (n_ % d_ == 0) {
      return {Integer(n_ / d_)};
    }
    return {};
  }

  // Normalize a rational into a whole integer part, and a rational whose absolute value is less
  // than one.
  std::pair<Integer, Rational> Normalize() const {
    return std::make_pair(Integer{n_ / d_}, Rational{n_ % d_, d_});
  }

  // True if the absolute value of the fraction is less than one.
  bool IsNormalized() const { return std::abs(n_) < d_; }

  // Create a rational expression and simplify if possible.
  static Expr Create(Rational r);

  // Create a rational expression and simplify if possible.
  static Expr Create(IntegralType n, IntegralType d) { return Create(Rational{n, d}); }

 private:
  IntegralType n_;
  IntegralType d_;
};

// A floating point constant.
class Float : public ExpressionImpl<Float> {
 public:
  using FloatType = double;

  // Construct from float value.
  explicit Float(FloatType val) : val_(val) {
    ASSERT(std::isfinite(val_), "Float values must be finite: val = {}", val_);
  }

  // Check if numerical constants are completely identical.
  bool IsIdenticalToImplTyped(const Float& other) const { return val_ == other.val_; }

  // Access numeric value.
  FloatType GetValue() const { return val_; }

  // Get absolute value.
  Float Abs() const { return Float{std::abs(val_)}; }

  // Create floating point expression.
  static Expr Create(FloatType f) { return MakeExpr<Float>(f); }

 private:
  FloatType val_;
};

// Operations on integers:
inline auto operator*(const Integer& a, const Integer& b) {
  return Integer{a.GetValue() * b.GetValue()};
}
inline auto operator+(const Integer& a, const Integer& b) {
  return Integer{a.GetValue() + b.GetValue()};
}
inline bool operator<(const Integer& a, const Integer& b) { return a.GetValue() < b.GetValue(); }
inline bool operator==(const Integer& a, const Integer& b) { return a.GetValue() == b.GetValue(); }

inline Integer::operator Float() const { return Float{static_cast<Float::FloatType>(val_)}; }
inline Integer::operator Rational() const { return Rational{GetValue(), 1}; }

// Operations on rationals:
inline auto operator*(const Rational& a, const Rational& b) {
  return Rational{a.Numerator() * b.Numerator(), a.Denominator() * b.Denominator()};
}
inline auto operator+(const Rational& a, const Rational& b) {
  // Create common denominator and create a new rational:
  return Rational{a.Numerator() * b.Denominator() + b.Numerator() * a.Denominator(),
                  a.Denominator() * b.Denominator()};
}
inline auto operator-(const Rational& a, const Rational& b) {
  return Rational{a.Numerator() * b.Denominator() - b.Numerator() * a.Denominator(),
                  a.Denominator() * b.Denominator()};
}
inline bool operator<(const Rational& a, const Rational& b) {
  // TODO: Watch for overflow.
  return a.Numerator() * b.Denominator() < b.Numerator() * a.Denominator();
}
inline bool operator==(const Rational& a, const Rational& b) {
  // Constructor ensures we reduce to common denominator, so we can compare directly.
  return a.Numerator() == b.Numerator() && a.Denominator() == b.Denominator();
}

inline Rational::operator Float() const {
  // TODO: Look up if there is a more accurate way of doing this.
  return Float{static_cast<Float::FloatType>(n_) / static_cast<Float::FloatType>(d_)};
}

// Wrap an angle specified as a rational multiple of pi into the range (-pi, pi]. A new rational
// coefficient between (-1, 1] is returned.
inline Rational ModPiRational(const Rational& r) {
  // Split into integer and rational parts:
  const auto [integer_part_unwrapped, fractional_part] = r.Normalize();
  // Wrap the integer part into [-2, 2].
  const int64_t integer_part = integer_part_unwrapped.GetValue() % 2;
  // Now we want to convert into range [-1, 1]:
  if (integer_part == 1) {
    return fractional_part.IsZero() ? Rational{1, 1} : fractional_part - Rational{1, 1};
  } else if (integer_part == -1) {
    return Rational{1, 1} + fractional_part;
  }
  return fractional_part;
}

// Operations on floats:
inline auto operator*(const Float& a, const Float& b) { return Float{a.GetValue() * b.GetValue()}; }
inline auto operator+(const Float& a, const Float& b) { return Float{a.GetValue() + b.GetValue()}; }
inline bool operator<(const Float& a, const Float& b) { return a.GetValue() < b.GetValue(); }

// Will evaluate to true if A or B (or both) is a float, w/ the other being Integer or Rational.
// This is so we can promote integers/rationals -> float when they are combined with floats.
template <typename A, typename B>
constexpr bool IsFloatAndNumeric =
    (std::is_same_v<A, Float> && ContainsTypeHelper<B, Integer, Rational>) ||
    (std::is_same_v<B, Float> && ContainsTypeHelper<A, Integer, Rational>) ||
    (std::is_same_v<A, Float> && std::is_same_v<B, Float>);

static_assert(IsFloatAndNumeric<Float, Float>);
static_assert(IsFloatAndNumeric<Float, Integer>);
static_assert(IsFloatAndNumeric<Rational, Float>);
static_assert(!IsFloatAndNumeric<Integer, Integer>);
static_assert(!IsFloatAndNumeric<Integer, Rational>);

}  // namespace math
