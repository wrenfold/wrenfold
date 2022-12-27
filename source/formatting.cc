#include "formatting.h"

#include <fmt/format.h>

#include <optional>
#include <type_traits>

#include "binary_operations.h"
#include "constant_expressions.h"
#include "unary_operations.h"
#include "variable.h"

namespace math {

static int GetPrecedence(const ExpressionBase& expr) {
  const OperationBase* const op = expr.As<OperationBase>();
  if (op) {
    return op->Precedence();
  }
  return std::numeric_limits<int>::max();
}

// Helper for appropriately applying brackets, considering operator precedence.
void PlainFormatter::FormatBinaryOp(const int parent_precedence, const char* const op_str,
                                    const ExpressionBase& a, const ExpressionBase& b) {
  if (parent_precedence > GetPrecedence(a) || !use_precedence_) {
    output_ += "(";
    a.Receive(*this);
    output_ += ")";
  } else {
    a.Receive(*this);
  }
  fmt::format_to(std::back_inserter(output_), " {} ", op_str);
  if (parent_precedence > GetPrecedence(b) || !use_precedence_) {
    output_ += "(";
    b.Receive(*this);
    output_ += ")";
  } else {
    b.Receive(*this);
  }
}

void PlainFormatter::Apply(const Addition& expr) {
  FormatBinaryOp(Addition::OperatorPrecedence, "+", *expr.First(), *expr.Second());
}

void PlainFormatter::Apply(const Constant& expr) {
  switch (expr.GetName()) {
    case SymbolicConstants::Pi:
      output_ += "pi";
      break;
    case SymbolicConstants::Euler:
      output_ += "e";
      break;
    default:
      output_ += "<UNKNOWN CONSTANT>";
      break;
  }
}

void PlainFormatter::Apply(const Division& expr) {
  FormatBinaryOp(Division::OperatorPrecedence, "/", *expr.First(), *expr.Second());
}

void PlainFormatter::Apply(const Multiplication& expr) {
  FormatBinaryOp(Multiplication::OperatorPrecedence, "*", *expr.First(), *expr.Second());
}

void PlainFormatter::Apply(const NaturalLog& expr) {
  output_ += "ln(";
  expr.Inner()->Receive(*this);
  output_ += ")";
}

// Template to check if the `Apply` method is implemented.
template <typename Handler, typename, typename = void>
constexpr bool HandlerCompiles = false;

// Specialization that is activated when the Apply method exists:
template <typename Handler, typename Argument>
constexpr bool HandlerCompiles<
    Handler, Argument, decltype(std::declval<Handler>()(std::declval<const Argument>()), void())> =
    true;

template <typename ReturnType, typename HandlerType>
struct TemporaryVisitor
    : public VisitorWithoutResultImpl<TemporaryVisitor<ReturnType, HandlerType>> {
 public:
  TemporaryVisitor(HandlerType&& handler) : handler(std::move(handler)) {}

  template <typename Argument>
  std::enable_if_t<HandlerCompiles<HandlerType, Argument>, void> Apply(const Argument& arg) {
    result = handler(arg);
  }

  HandlerType handler{};
  std::optional<ReturnType> result{};
};

template <typename HandlerType>
auto Visit(const ExpressionBaseConstPtr& expr, HandlerType handler) {
  using ReturnType = typename HandlerType::ReturnType;
  TemporaryVisitor<ReturnType, HandlerType> visitor{std::move(handler)};
  expr->Receive(visitor);
  return visitor.result;
}

struct NeedsBracketsVisitor {
  using ReturnType = bool;
  // If the operation is a binary op, we need brackets.
  template <typename Anything>
  constexpr bool operator()(const Anything&) {
    return false;
  }

  //  constexpr bool operator()(const )
};

void PlainFormatter::Apply(const Negation& expr) {
  output_ += "-";

  const std::optional<bool> needs_brackets = Visit(expr.Inner(), NeedsBracketsVisitor{});
  if (needs_brackets.has_value() && *needs_brackets) {
    output_ += "(";
    expr.Inner()->Receive(*this);
    output_ += ")";
  } else {
    expr.Inner()->Receive(*this);
  }
}

void PlainFormatter::Apply(const Number& expr) {
  fmt::format_to(std::back_inserter(output_), "{}", expr.GetValue());
}

void PlainFormatter::Apply(const Power& expr) {
  output_ += "pow(";
  expr.First()->Receive(*this);
  output_ += ", ";
  expr.Second()->Receive(*this);
  output_ += ")";
}

void PlainFormatter::Apply(const Subtraction& expr) {
  FormatBinaryOp(Subtraction::OperatorPrecedence, "-", *expr.First(), *expr.Second());
}

void PlainFormatter::Apply(const Variable& expr) { output_ += expr.GetName(); }

}  // namespace math
