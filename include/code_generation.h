// Copyright 2023 Gareth Cross
#include "expression.h"

#include <variant>
#include <vector>

#include "expressions/function_expressions.h"  //  temporary

namespace math {
namespace ssa {

// Operands are either the result of an assignment (size_t), or an input value
// (in which case we just use Expr).
using Operand = std::variant<std::size_t, Expr>;

struct Add {
  constexpr std::string_view ToString() const { return "add"; }
  constexpr bool IsCommutative() const { return true; }

  //  Operand left;
  //  Operand right;
};

struct Mul {
  constexpr std::string_view ToString() const { return "mul"; }
  constexpr bool IsCommutative() const { return true; }

  //  Operand left;
  //  Operand right;
};

struct Pow {
  constexpr std::string_view ToString() const { return "pow"; }
  constexpr bool IsCommutative() const { return false; }

  //  Operand base;
  //  Operand exponent;
};

struct Load {
  constexpr std::string_view ToString() const { return "load"; }
  constexpr bool IsCommutative() const { return false; }

  //  Expr input;
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc {
  constexpr std::string_view ToString() const { return math::ToString(name); }
  constexpr bool IsCommutative() const { return false; }

  UnaryFunctionName name;
  //  Operand arg;
};

using OperationVariant = std::variant<Add, Mul, Pow, CallUnaryFunc>;

struct Operation {
  OperationVariant op_type{};
  std::vector<std::size_t> args;  //  TODO: Small vector
};

struct InputValue {
  Expr value;
};

using AssignmentVariant = std::variant<Operation, InputValue>;

}  // namespace ssa

// Create "static single assignment" form from an expression tree.
// Expression tree must be a scalar (not matrix).
void CreateSSA(const Expr& expression, std::vector<ssa::AssignmentVariant>& output);

// Print a list of assignments.
std::string FormatSSA(const std::vector<ssa::AssignmentVariant>& assignments);

// Re-create expression tree from assignments.
Expr ExpressionFromSSA(const std::vector<ssa::AssignmentVariant>& assignments);

//
std::vector<ssa::AssignmentVariant> EliminateDuplicates(
    const std::vector<ssa::AssignmentVariant>& assignments);

}  // namespace math
