// Copyright 2022 Gareth Cross
#pragma once
#include <cmath>
#include <numeric>
#include <optional>
#include <vector>

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

  // Create an integer expression.
  static Expr Create(IntegralType x);

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

  // True if the numerator is zero.
  bool IsZero() const { return n_ == 0; }

  // Try converting the rational to an integer. If the numerator and denominator divide
  // evenly, returns a valid optional.
  std::optional<Integer> TryConvertToInteger() const {
    if (n_ % d_ == 0) {
      return {Integer(n_ / d_)};
    }
    return {};
  }

  // Create a rational expression and simplify if possible.
  static Expr Create(Rational r);

  // Create a rational expression and simplify if possible.
  static Expr Create(IntegralType n, IntegralType d) { return Create(Rational{n, d}); }

 private:
  IntegralType n_;
  IntegralType d_;
};

//// A number factorized into "Proper Exponent Primal" form.
//// See https://arxiv.org/pdf/1302.2169.pdf
// class FactorizedNumber : public ExpressionImpl<FactorizedNumber> {
//  public:
//   // A factor in the number.
//   struct Factor {
//     int64_t base;  // TODO: Use Integer?
//     Rational exponent;
//   };
//
//   // Order factors by their base.
//   struct FactorBaseOrder {
//     bool operator()(const Factor& a, const Factor& b) const { return a.base < b.base; }
//   };
//
//   // Construct from an integer (factors are left empty).
//   explicit FactorizedNumber(int64_t integer) : coeff_{integer, 1} {}
//
//   // Raise to a rational power:
//   FactorizedNumber Pow(const Rational& exp) const;
//
//   // Check for exact equality.
//   bool IsIdenticalToImplTyped(const FactorizedNumber& other) const {
//     if (!coeff_.IsIdenticalToImplTyped(other.coeff_) || factors_.size() != other.factors_.size())
//     {
//       return false;
//     }
//     return std::equal(factors_.begin(), factors_.end(), other.factors_.begin(),
//                       [](const Factor& a, const Factor& b) {
//                         return a.base == b.base && a.exponent.IsIdenticalToImplTyped(b.exponent);
//                       });
//   }
//
//  protected:
//   // Construct w/ coefficient and factors:
//   FactorizedNumber(const Rational& coeff, std::vector<Factor> factors)
//       : coeff_(coeff), factors_(std::move(factors)) {}
//
//   friend FactorizedNumber operator*(const FactorizedNumber&, const FactorizedNumber&);
//
//   // Rational coefficient in front of the prime factors.
//   Rational coeff_;
//
//   // TODO: Use small vector.
//   std::vector<Factor> factors_;
// };

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

inline Rational::operator Float() const {
  // TODO: Look up if there is a more accurate way of doing this.
  return Float{static_cast<Float::FloatType>(n_) / static_cast<Float::FloatType>(d_)};
}

// Operations on floats:
inline auto operator*(const Float& a, const Float& b) { return Float{a.GetValue() * b.GetValue()}; }
inline auto operator+(const Float& a, const Float& b) { return Float{a.GetValue() + b.GetValue()}; }

}  // namespace math
