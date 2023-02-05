// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_impl.h"

namespace math {

// How to print the pow() operation:
enum class PowerStyle {
  // Use the ^ operator.
  Hat,
  // Use the python ** operator.
  Python,
};

// Simple plain-text formatter.
class PlainFormatter : public VisitorImpl<PlainFormatter, void> {
 public:
  using ReturnType = void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::CompileError;

  PlainFormatter() = default;
  PlainFormatter(PowerStyle style) : power_style_(style) {}

  void Apply(const Addition& add);
  void Apply(const Constant& constant);
  void Apply(const Float& num);
  void Apply(const Integer& num);
  void Apply(const Multiplication& mul);
  void Apply(const Power& pow);
  void Apply(const Rational& rational);
  void Apply(const UnaryFunction& func);
  void Apply(const Variable& var);

  // Get the output string.
  const std::string& GetOutput() const { return output_; }

 private:
  // Wrap `expr` in braces if the precedence is <= the parent.
  void FormatPrecedence(Precedence parent, const Expr& expr);

  // Format power operation with the appropriate operator.
  void FormatPower(const Expr& Base, const Expr& Exponent);

  std::string output_{};
  PowerStyle power_style_{PowerStyle::Hat};
};

}  // namespace math
