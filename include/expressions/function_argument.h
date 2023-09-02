// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "expression_impl.h"

namespace math {

// A type that is substituted into user-provided expressions during code-generation.
// Typically, the user does not create these directly.
class FunctionArgument {
 public:
  constexpr static std::string_view NameStr = "FunctionArgument";
  constexpr static bool IsLeafNode = true;

  FunctionArgument(std::size_t arg_index, std::size_t element_index)
      : arg_index_(arg_index), element_index_(element_index) {}

  constexpr bool IsIdenticalTo(const FunctionArgument& other) const {
    return arg_index_ == other.arg_index_ && element_index_ == other.element_index_;
  }

  std::size_t ArgIndex() const { return arg_index_; }
  std::size_t ElementIndex() const { return element_index_; }

  // Create `Expr` w/ the specified indices.
  static Expr Create(std::size_t arg_index, std::size_t element_index) {
    return MakeExpr<FunctionArgument>(arg_index, element_index);
  }

  // Equal if both indices are the same.
  constexpr bool operator==(const FunctionArgument& other) const { return IsIdenticalTo(other); }

 private:
  std::size_t arg_index_;
  std::size_t element_index_;
};

}  // namespace math
