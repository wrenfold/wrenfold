// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_impl.h"

namespace math {

// Simple plain-text formatter.
class PlainFormatter : public VisitorWithoutResultImpl<PlainFormatter> {
 public:
  PlainFormatter(bool use_precedence = true) : use_precedence_(use_precedence) {}

  void Apply(const Addition& add);
  void Apply(const Constant& constant);
  void Apply(const Division& div);
  void Apply(const Multiplication& mul);
  void Apply(const NaturalLog& log);
  void Apply(const Negation& neg);
  void Apply(const Number& num);
  void Apply(const Power& pow);
  void Apply(const Subtraction& sub);
  void Apply(const Variable& var);

  // Get the output string.
  const std::string& GetOutput() const { return output_; }

 private:
  // Format a binary operation.
  void FormatBinaryOp(int parent_precedence, const char* op_str, const Expr& a, const Expr& b);

  std::string output_{};
  bool use_precedence_;
};

}  // namespace math
