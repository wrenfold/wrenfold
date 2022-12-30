#pragma once
#include "expression_concept.h"
#include "expression_impl.h"
#include "formatting.h"

namespace math {

/**
 * A variable w/ a string name.
 */
class Variable : public ExpressionImpl<Variable> {
 public:
  explicit Variable(std::string name) : name_(std::move(name)) {}

  // Check if two variables are the same (names match).
  bool IsIdenticalToImplTyped(const Variable& other) const { return name_ == other.name_; }

  // Get variable name
  const std::string& GetName() const { return name_; }

 private:
  std::string name_;
};

}  // namespace math
