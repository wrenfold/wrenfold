// Copyright 2023 Gareth Cross
#include "expression.h"

#include <unordered_map>
#include <variant>
#include <vector>

#include "expressions/function_expressions.h"  //  temporary
#include "ordering.h"

namespace math {
namespace ssa {

// Operands are either the result of an assignment (size_t), or an input value
// (in which case we just use Expr).
using Operand = std::variant<std::size_t, Expr>;

inline void OrderOperands(Operand& left, Operand& right) {
  struct Visitor {
    // Order assignments by index.
    bool operator()(std::size_t l, std::size_t r) const { return l < r; }

    // Order assignment operands before input value operands.
    constexpr bool operator()(std::size_t, const Expr&) const { return true; }
    constexpr bool operator()(const Expr&, std::size_t) const { return false; }

    // Order input value operands as we typically would.
    bool operator()(const Expr& l, const Expr& r) const {
      return VisitBinaryStruct(l, r, OrderVisitor{}) == OrderVisitor::RelativeOrder::LessThan;
    }
  };
  const bool is_ordered = std::visit(Visitor{}, left, right);
  if (!is_ordered) {
    std::swap(left, right);
  }
}

struct Add {
  constexpr std::string_view ToString() const { return "add"; }

  Add(Operand left, Operand right) : args{std::move(left), std::move(right)} {
    OrderOperands(args[0], args[1]);
  }

  std::array<Operand, 2> args;
};

struct Mul {
  constexpr std::string_view ToString() const { return "mul"; }

  Mul(Operand left, Operand right) : args{std::move(left), std::move(right)} {
    OrderOperands(args[0], args[1]);
  }

  std::array<Operand, 2> args;
};

struct Pow {
  constexpr std::string_view ToString() const { return "pow"; }

  Pow(Operand base, Operand exponent) : args{std::move(base), std::move(exponent)} {}

  std::array<Operand, 2> args;
};

struct Load {
  constexpr std::string_view ToString() const { return "load"; }

  explicit Load(Expr input) : args{std::move(input)} {}
  explicit Load(std::size_t input) : args{Operand(input)} {}

  std::array<Operand, 1> args;
};

// TODO: Should probably just support n-ary functions w/ one object.
struct CallUnaryFunc {
  constexpr std::string_view ToString() const { return math::ToString(name); }

  CallUnaryFunc(UnaryFunctionName name, Operand arg) : name(name), args{std::move(arg)} {}

  UnaryFunctionName name;
  std::array<Operand, 1> args;
};

using OperationVariant = std::variant<Add, Mul, Pow, Load, CallUnaryFunc>;

}  // namespace ssa

// Create "static single assignment" form from an expression tree.
// Expression tree must be a scalar (not matrix).
std::size_t CreateSSA(const Expr& expression,
                      std::unordered_map<std::size_t, ssa::OperationVariant>& output);

// Print a list of assignments.
// std::string FormatSSA(const std::vector<ssa::OperationVariant>& operations);

// Print a list of assignments.
std::string FormatSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations);

// Re-create expression tree from assignments.
Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       std::size_t value_index);
Expr ExpressionFromSSA(const std::unordered_map<std::size_t, ssa::OperationVariant>& operations,
                       const std::size_t value_index,
                       std::unordered_map<std::size_t, Expr>& output_map);

//
void EliminateDuplicates(std::unordered_map<std::size_t, ssa::OperationVariant>& operations);

}  // namespace math
