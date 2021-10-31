#pragma once
#include "expression_base.h"

namespace math {

/**
 * A variable w/ a string name.
 *
 * TODO(gareth): Use wide-string here.
 */
class Variable : public ExpressionImpl<Variable> {
 public:
  explicit Variable(const std::string& name) : name_(name) {}

  // Differentiate a variable wrt another, producing either one or zero.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  // Convert to string.
  std::string Format() const override { return name_; }

  // Check if two variables are the same (names match).
  bool IsIdenticalToImplTyped(const Variable& other) const;

 private:
  std::string name_;
};

}  // namespace math
