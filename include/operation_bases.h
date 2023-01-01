// Copyright 2022 Gareth Cross
#pragma once
#include <vector>

#include "expression_impl.h"

namespace math {

// Operation that has one argument.
template <typename Derived>
class UnaryOp : public ExpressionImpl<Derived> {
 public:
  explicit UnaryOp(const Expr& x) : x_(x) {}
  explicit UnaryOp(Expr&& x) : x_(std::move(x)) {}

  // Test unary ops for equality.
  bool IsIdenticalToImplTyped(const UnaryOp<Derived>& neg) const {
    return x_.IsIdenticalTo(neg.x_);
  }

  // Get inner expression.
  const Expr& Inner() const { return x_; }

 protected:
  Expr x_;
};

// Operation that has two arguments.
template <typename Derived>
class BinaryOp : public ExpressionImpl<Derived> {
 public:
  BinaryOp(Expr first, Expr second) : first_(std::move(first)), second_(std::move(second)) {}

  // Test binary ops for equality.
  bool IsIdenticalToImplTyped(const BinaryOp<Derived>& other) const {
    return first_.IsIdenticalTo(other.first_) && second_.IsIdenticalTo(other.second_);
  }

  const Expr& First() const { return first_; }
  const Expr& Second() const { return second_; }

 protected:
  Expr first_;
  Expr second_;
};

// Operation that has `N` arguments.
template <typename Derived>
class NAryOp : public ExpressionImpl<Derived> {
 public:
  // Construct via move.
  explicit NAryOp(std::vector<Expr>&& args) : args_(std::move(args)) {}

  NAryOp() = default;

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return args_[i]; }

  // Get all the args.
  const std::vector<Expr>& Args() const { return args_; }

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
  // TODO: Should try absl::InlineVector for this.
  std::vector<Expr> args_;
};

}  // namespace math
