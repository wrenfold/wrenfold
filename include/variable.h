#pragma once
#include "expression_base.h"
#include "formatting.h"

namespace math {

/**
 * A variable w/ a string name.
 */
class Variable : public ExpressionImpl<Variable> {
 public:
  explicit Variable(std::string name) : name_(std::move(name)) {}

  // Differentiate a variable wrt another, producing either one or zero.
  ExpressionBaseConstPtr Diff(const Variable& var) const override;

  // Check if two variables are the same (names match).
  bool IsIdenticalToImplTyped(const Variable& other) const;

  // Get variable name
  const std::string& GetName() const { return name_; }

 private:
  std::string name_;
};

}  // namespace math
