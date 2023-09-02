// Copyright 2022 Gareth Cross
#pragma once
#include <array>
#include <vector>

#include "assertions.h"
#include "expression_impl.h"

namespace math {

// Operation that has `N` arguments.
template <typename Derived>
class NAryOp {
 public:
  using ContainerType = std::vector<Expr>;
  static constexpr bool IsLeafNode = false;

  // ConstructMatrix via move.
  explicit NAryOp(ContainerType&& args) : args_(std::move(args)) {}

  NAryOp() = default;

  // Access specific argument.
  const Expr& operator[](const std::size_t i) const { return args_[i]; }

  // Get all the args.
  const ContainerType& Args() const { return args_; }

  // Number of arguments.
  std::size_t Arity() const { return args_.size(); }

  // Iterators.
  ContainerType::const_iterator begin() const { return args_.begin(); }
  ContainerType::const_iterator end() const { return args_.end(); }

  // Name of the operation.
  constexpr std::string_view Name() const { return Derived::NameStr; }

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool IsIdenticalToImplTyped(const NAryOp<Derived>& other) const {
    if (Arity() != other.Arity()) {
      return false;
    }
    return std::equal(args_.begin(), args_.end(), other.args_.begin(),
                      [](const Expr& x, const Expr& y) { return x.IsIdenticalTo(y); });
  }

  // Implement ExpressionImpl::Iterate
  template <typename Operation>
  void Iterate(Operation operation) const {
    std::for_each(args_.begin(), args_.end(), std::move(operation));
  }

  // Implement ExpressionImpl::Map
  template <typename Operation>
  Expr Map(Operation operation) const {
    std::vector<Expr> transformed;
    transformed.reserve(Arity());
    std::transform(args_.begin(), args_.end(), std::back_inserter(transformed),
                   std::move(operation));
    return Derived::FromOperands(transformed);  //  TODO: should be a move
  }

 protected:
  // TODO: Should try absl::InlineVector for this.
  ContainerType args_;
};

}  // namespace math
