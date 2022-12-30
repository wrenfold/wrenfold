#pragma once
#include <algorithm>
#include <vector>

#include "constants.h"
#include "expression.h"
#include "expression_impl.h"
#include "visitor_impl.h"

// TODO: These are all n-ary ops (except for power), and should changed.
namespace math {

// Operation that has
template <typename Derived>
class NAryOp : public ExpressionImpl<Derived> {
 public:
  // Construct via move.
  explicit NAryOp(std::vector<Expr>&& args) : args_(std::move(args)) {}

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return args_[i]; }

  // Number of arguments.
  std::size_t Arity() const { return args_.size(); }

  // Name of the operation.
  constexpr const char* Name() const { return Derived::NameStr; }

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool IsIdenticalToImplTyped(const NAryOp<Derived>& other) const {
    if (Arity() != other.Arity()) {
      return false;
    }
    return std::equal(args_.begin(), args_.end(), other.args_.begin(),
                      [](const Expr& x, const Expr& y) { return x.IsIdenticalTo(y); });
  }

 protected:
  // TODO: Could specialize for small (like binary) cases?
  // Try absl::InlineVector.
  std::vector<Expr> args_;
};

class Addition : public NAryOp<Addition> {
 public:
  using NAryOp::NAryOp;
  static constexpr const char* NameStr = "Addition";

  // Construct from two operands.
  // Templated so we can forward r-value references.
  static Expr FromTwoOperands(const Expr& a, const Expr& b) {
    // Check if either argument is zero:
    if (IsZero(a)) {
      return b;
    } else if (IsZero(b)) {
      return a;
    }

    // TODO: Clean this up...
    const Addition* a_add = TryCast<Addition>(a);
    const Addition* b_add = TryCast<Addition>(b);

    std::vector<Expr> args;
    args.reserve(2);
    if (a_add) {
      args.insert(args.end(), a_add->args_.begin(), a_add->args_.end());
    } else {
      args.push_back(a);
    }
    if (b_add) {
      args.insert(args.end(), b_add->args_.begin(), b_add->args_.end());
    } else {
      args.push_back(b);
    }
    return MakeExpr<Addition>(std::move(args));
  }
};

class Multiplication : public NAryOp<Multiplication> {
 public:
  static constexpr const char* NameStr = "Multiplication";

  // TODO: Re-write args in order?
  explicit Multiplication(std::vector<Expr> args) : NAryOp(std::move(args)) {}

  static Expr FromTwoOperands(const Expr& a, const Expr& b) {
    // Check if either argument is zero:
    if (IsZero(a) || IsZero(b)) {
      return Constants::Zero;
    }
    if (IsOne(a)) {
      return b;
    }
    if (IsOne(b)) {
      return a;
    }

    const Multiplication* a_mul = TryCast<Multiplication>(a);
    const Multiplication* b_mul = TryCast<Multiplication>(b);

    // a * b * c * d -> Multiplication(a, b, c, d)
    // a * b * (c + d) -> Multiplication(a, b, c + d)
    // (a + b) * (c + d) -> Multiplication(a + b, c + d)

    std::vector<Expr> args;
    args.reserve(2);
    if (a_mul) {
      args.insert(args.end(), a_mul->args_.begin(), a_mul->args_.end());
    } else {
      args.push_back(a);
    }
    if (b_mul) {
      args.insert(args.end(), b_mul->args_.begin(), b_mul->args_.end());
    } else {
      args.push_back(b);
    }
    return MakeExpr<Multiplication>(std::move(args));
  }
};

// Division operation.
// TODO: Delete me and just use multiplication.
class Division : public ExpressionImpl<Division> {
 public:
  Division(const Expr& numerator, const Expr& denominator) : num_(numerator), den_(denominator) {}

  bool IsIdenticalToImplTyped(const Division& other) const {
    return num_.IsIdenticalTo(other.num_) && den_.IsIdenticalTo(other.den_);
  }

  const Expr& Numerator() const { return num_; }
  const Expr& Denominator() const { return den_; }

 protected:
  Expr num_;
  Expr den_;
};

// Power operation: base^exponent
class Power : public ExpressionImpl<Power> {
 public:
  // TODO: Add move constructor.
  Power(const Expr& base, const Expr& exponent) : base_(base), exponent_(exponent) {}

  static Expr Create(const Expr& a, const Expr& b) {
    //    ASSERT(!IsZero(a) || !IsZero(b), "TODO: Implement proper handling of 0^0");
    if (IsZero(a)) {
      return Constants::Zero;
    }
    if (IsZero(b)) {
      return Constants::One;
    }
    if (IsOne(b)) {
      return a;
    }
    return MakeExpr<Power>(a, b);
  }

  bool IsIdenticalToImplTyped(const Power& other) const {
    return base_.IsIdenticalTo(other.base_) && exponent_.IsIdenticalTo(other.exponent_);
  }

  const Expr& Base() const { return base_; }
  const Expr& Exponent() const { return exponent_; }

 protected:
  Expr base_;
  Expr exponent_;
};

}  // namespace math
