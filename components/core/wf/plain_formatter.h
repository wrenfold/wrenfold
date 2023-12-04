// Copyright 2022 Gareth Cross
#pragma once
#include "wf/visitor_impl.h"

namespace math {

// Simple plain-text formatter.
class plain_formatter {
 public:
  void operator()(const addition& add);
  void operator()(const cast_bool& cast);
  void operator()(const conditional& conditional);
  void operator()(const Constant& constant);
  void operator()(const derivative& derivative);
  void operator()(const Float& num);
  void operator()(const Infinity&);
  void operator()(const Integer& num);
  void operator()(const class matrix& mat);
  void operator()(const multiplication& mul);
  void operator()(const Power& pow);
  void operator()(const Rational& rational);
  void operator()(const Relational& relational);
  void operator()(const function& func);
  void operator()(const Undefined&);
  void operator()(const Variable& var);

  // Get the output string (transferring ownership to the caller).
  std::string take_output() const { return std::move(output_); }

 private:
  // Wrap `expr` in braces if the precedence is <= the parent.
  void format_precedence(precedence parent, const Expr& expr);

  // Format power operation with the appropriate operator.
  void format_power(const Expr& Base, const Expr& Exponent);

  std::string output_{};
};

}  // namespace math
