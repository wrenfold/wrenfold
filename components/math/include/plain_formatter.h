// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_impl.h"

namespace math {

// Simple plain-text formatter.
class PlainFormatter {
 public:
  void operator()(const Addition& add);
  void operator()(const Conditional& conditional);
  void operator()(const Constant& constant);
  void operator()(const Derivative& derivative);
  void operator()(const Float& num);
  void operator()(const FunctionArgument& func_arg);
  void operator()(const Infinity&);
  void operator()(const Integer& num);
  void operator()(const Matrix& mat);
  void operator()(const Multiplication& mul);
  void operator()(const Power& pow);
  void operator()(const Rational& rational);
  void operator()(const Relational& relational);
  void operator()(const Function& func);
  void operator()(const Variable& var);

  // Get the output string.
  const std::string& GetOutput() const { return output_; }

 private:
  // Wrap `expr` in braces if the precedence is <= the parent.
  void FormatPrecedence(Precedence parent, const Expr& expr);

  // Format power operation with the appropriate operator.
  void FormatPower(const Expr& Base, const Expr& Exponent);

  std::string output_{};
};

}  // namespace math