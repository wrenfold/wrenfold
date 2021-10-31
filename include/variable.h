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
  std::string ToString() const override { return name_; }

  // Check if two variables are the same (names match).
  bool EqualsImplTyped(const Variable& other) const;

 private:
  std::string name_;
};

// Make a variable from a string.
inline Expr MakeVar(const std::string& name) { return Expr{std::make_shared<Variable>(name)}; }

}  // namespace math
