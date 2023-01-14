// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_impl.h"

namespace math {

// Simple plain-text formatter.
class PlainFormatter : public VisitorImpl<PlainFormatter, void> {
 public:
  using ReturnType = void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::CompileError;

  void Apply(const Addition& add);
  void Apply(const Constant& constant);
  void Apply(const Float& num);
  void Apply(const Integer& num);
  void Apply(const Multiplication& mul);
  void Apply(const NaturalLog& log);
  void Apply(const Power& pow);
  void Apply(const Rational& rational);
  void Apply(const Variable& var);

  // Get the output string.
  const std::string& GetOutput() const { return output_; }

 private:
  void FormatPrecedence(Precedence parent, const Expr& expr);

  void FormatPower(const Expr& Base, const Expr& Exponent);

  std::string output_{};
};

}  // namespace math
