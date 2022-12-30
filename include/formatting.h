// Copyright 2022 Gareth Cross
#pragma once
#include "visitor_impl.h"

namespace math {

// Simple plain-text formatter.
class PlainFormatter : public VisitorImpl<PlainFormatter, void> {
 public:
  using ReturnType = Void;
  static constexpr VisitorPolicy Policy = VisitorPolicy::CompileError;

  void Apply(const Addition& add);
  void Apply(const Constant& constant);
  void Apply(const Float& num);
  void Apply(const Integer& num);
  void Apply(const Multiplication& mul);
  void Apply(const NaturalLog& log);
  void Apply(const Negation& neg);
  void Apply(const Power& pow);
  void Apply(const Variable& var);

  // Get the output string.
  const std::string& GetOutput() const { return output_; }

 private:
  void VisitWithBrackets(const Expr& expr);

  void FormatBaseExponentPair(const std::pair<Expr, Expr>& pair, bool denominator);

  std::string output_{};
};

std::string FormatDebugTree(const Expr& expr);

}  // namespace math
