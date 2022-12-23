#pragma once
#include "constants.h"
#include "expression_base.h"

namespace math {

/**
 * A scalar numerical constant like 0.23241, or 42.
 *
 * TODO(gareth): Add support for complex numbers.
 */
class Number : public ExpressionImpl<Number> {
 public:
  // TODO(gareth): Support integer types? For now double works fine.
  using NumberType = double;

  // Construct from number.
  explicit Number(NumberType val) : val_(val) {}

  // Check if numerical constants are completely identical.
  bool IsIdenticalToImplTyped(const Number& other) const { return val_ == other.val_; }

  // Access numeric value.
  NumberType GetValue() const { return val_; }

 private:
  NumberType val_;
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

}  // namespace math
