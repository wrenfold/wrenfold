// Copyright 2022 Gareth Cross
#pragma once
#include "constants.h"
#include "expression_impl.h"

namespace math {

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

// Order constants by their enum values.
inline bool operator<(const Constant& a, const Constant& b) { return a.GetName() < b.GetName(); }

}  // namespace math
