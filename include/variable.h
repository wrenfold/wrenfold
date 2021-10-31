#pragma once
#include "expression_base.h"
#include "formatting.h"

namespace math {

/**
 * A variable w/ a string name.
 */
class Variable : public ExpressionImpl<Variable> {
 public:
  explicit Variable(const StringType& name) : name_(name) {}

  // Differentiate a variable wrt another, producing either one or zero.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  // Check if two variables are the same (names match).
  bool IsIdenticalToImplTyped(const Variable& other) const;

  // Get variable name
  const StringType& GetName() const { return name_; }

 private:
  StringType name_;
};

}  // namespace math
