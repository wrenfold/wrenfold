// Copyright 2022 Gareth Cross
#pragma once

namespace math {

class OperationBase {
 public:
  virtual int Precedence() const = 0;
  virtual ~OperationBase() = default;
};

template <typename Derived>
class OperationImpl : public OperationBase {
 public:
  int Precedence() const override { return Derived::OperatorPrecedence; }
  virtual ~OperationImpl() = default;
};

}  // namespace math
