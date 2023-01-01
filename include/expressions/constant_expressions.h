#pragma once
#include <cmath>

#include "assertions.h"
#include "constants.h"
#include "expression_concept.h"
#include "expression_impl.h"

namespace math {

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

 private:
  IntegralType val_;
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

  // True if the stored value is actually an integer.
  bool IsActuallyInt() const { return std::ceil(val_) == val_; }

  // Cast to integer:
  explicit operator Integer() const;

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
inline Float::operator Integer() const { return Integer{static_cast<Integer::IntegralType>(val_)}; }

// Operations on floats:
inline auto operator*(const Float& a, const Float& b) { return Float{a.GetValue() * b.GetValue()}; }
inline auto operator+(const Float& a, const Float& b) { return Float{a.GetValue() + b.GetValue()}; }
inline Integer::operator Float() const { return Float{static_cast<Float::FloatType>(val_)}; }

// A symbolic constant, like pi or euler's number.
class Constant : public ExpressionImpl<Constant> {
 public:
  // Construct with name.
  explicit Constant(SymbolicConstants Name) : name_(Name) {}

  // Check if symbolic constants are the same.
  bool IsIdenticalToImplTyped(const Constant& other) const { return name_ == other.name_; }

  // Access name.
  SymbolicConstants GetName() const { return name_; }

 protected:
  SymbolicConstants name_;
};

// Convert symbolic constant enum to string constant.
// For debugging purposes.
inline const char* StringFromSymbolicConstant(SymbolicConstants value) {
  switch (value) {
    case SymbolicConstants::Pi:
      return "pi";
    case SymbolicConstants::Euler:
      return "e";
  }
}

}  // namespace math
