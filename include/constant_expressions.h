#pragma once
#include "constants.h"
#include "expression_concept.h"
#include "expression_impl.h"

namespace math {

/**
 * An integral constant.
 */
class Integer : public ExpressionImpl<Integer> {
 public:
  using IntegralType = int64_t;

  // Construct from number.
  explicit Integer(IntegralType val) : val_(val) {}

  // Check if numerical constants are completely identical.
  bool IsIdenticalToImplTyped(const Integer& other) const { return val_ == other.val_; }

  // Access numeric value.
  IntegralType GetValue() const { return val_; }

 private:
  IntegralType val_;
};

class Float : public ExpressionImpl<Float> {
 public:
  using FloatType = double;

  // Construct from float value.
  explicit Float(FloatType val) : val_(val) {}

  // Check if numerical constants are completely identical.
  bool IsIdenticalToImplTyped(const Float& other) const { return val_ == other.val_; }

  // Access numeric value.
  FloatType GetValue() const { return val_; }

 private:
  FloatType val_;
};

/*
 * A symbolic constant, like pi or euler's number.
 */
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
