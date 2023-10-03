#pragma once
#include "expression_concept.h"
#include "expression_impl.h"
#include "plain_formatter.h"

namespace math {

// A variable with a string name.
class Variable {
 public:
  static constexpr std::string_view NameStr = "Variable";
  static constexpr bool IsLeafNode = true;

  explicit Variable(std::string name) : name_(std::move(name)) {}

  // Check if two variables are the same (names match).
  bool is_identical_to(const Variable& other) const { return name_ == other.name_; }

  // Get variable name
  const std::string& name() const { return name_; }

 private:
  std::string name_;
};

template <>
struct hash_struct<Variable> {
  std::size_t operator()(const Variable& v) const { return hash_string_fnv(v.name()); }
};

}  // namespace math
