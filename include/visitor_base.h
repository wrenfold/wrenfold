// Copyright 2022 Gareth Cross
#pragma once
#include "expression_fwd.h"
#include "operations_fwd.h"

namespace math {

// Declare pure virtual Apply method.
// These are non-const, since visitors may need to cache or update internal values.
#define DECLARE_VIRTUAL_APPLY_METHOD(ArgType) \
  virtual ResultType ApplyVirtual(const ArgType& arg) = 0

// Implement override of the base method by calling the correct version for `ArgType`:
#define IMPLEMENT_VIRTUAL_APPLY_METHOD(ArgType) \
  ResultType ApplyVirtual(const ArgType& arg) override { return ApplyOrThrow(*this, arg); }

#define DECLARE_ALL_VIRTUAL_APPLY_METHODS()     \
  DECLARE_VIRTUAL_APPLY_METHOD(Addition);       \
  DECLARE_VIRTUAL_APPLY_METHOD(Constant);       \
  DECLARE_VIRTUAL_APPLY_METHOD(Division);       \
  DECLARE_VIRTUAL_APPLY_METHOD(Multiplication); \
  DECLARE_VIRTUAL_APPLY_METHOD(NaturalLog);     \
  DECLARE_VIRTUAL_APPLY_METHOD(Negation);       \
  DECLARE_VIRTUAL_APPLY_METHOD(Number);         \
  DECLARE_VIRTUAL_APPLY_METHOD(Power);          \
  DECLARE_VIRTUAL_APPLY_METHOD(Variable);

#define IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()     \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Addition);       \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Constant);       \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Division);       \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Multiplication); \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(NaturalLog);     \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Negation);       \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Number);         \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Power);          \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Variable);

// Base type for visitors that produce expressions.
class VisitorWithResultBase {
 public:
  using ResultType = ExpressionConceptConstPtr;
  DECLARE_ALL_VIRTUAL_APPLY_METHODS()
};

// Base type for visitors that do not produce expressions.
class VisitorWithoutResultBase {
 public:
  using ResultType = void;
  DECLARE_ALL_VIRTUAL_APPLY_METHODS()
};

}  // namespace math
