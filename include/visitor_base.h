// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

#include "expression_fwd.h"
#include "operations_fwd.h"
#include "template_utils.h"

namespace math {

// TODO: Allow switching this out for something w/o atomic operations?
using ExpressionConceptConstPtr = std::shared_ptr<const class ExpressionConcept>;

// Declare pure virtual Apply method.
// These are non-const, since visitors may need to cache or update internal values.
#define DECLARE_VIRTUAL_APPLY_METHOD(ArgType) virtual void ApplyVirtual(const ArgType& arg) = 0

// Implement override of the base method by calling the correct version for `ArgType`:
#define IMPLEMENT_VIRTUAL_APPLY_METHOD(ArgType) \
  void ApplyVirtual(const ArgType& arg) override { return ApplyOrThrow(*this, arg); }

#define DECLARE_ALL_VIRTUAL_APPLY_METHODS()       \
  DECLARE_VIRTUAL_APPLY_METHOD(Addition);         \
  DECLARE_VIRTUAL_APPLY_METHOD(Constant);         \
  DECLARE_VIRTUAL_APPLY_METHOD(Float);            \
  DECLARE_VIRTUAL_APPLY_METHOD(FunctionArgument); \
  DECLARE_VIRTUAL_APPLY_METHOD(Integer);          \
  DECLARE_VIRTUAL_APPLY_METHOD(Matrix);           \
  DECLARE_VIRTUAL_APPLY_METHOD(Multiplication);   \
  DECLARE_VIRTUAL_APPLY_METHOD(Power);            \
  DECLARE_VIRTUAL_APPLY_METHOD(Rational);         \
  DECLARE_VIRTUAL_APPLY_METHOD(UnaryFunction);    \
  DECLARE_VIRTUAL_APPLY_METHOD(Variable);

#define IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()       \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Addition);         \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Constant);         \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Float);            \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(FunctionArgument); \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Integer);          \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Matrix);           \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Multiplication);   \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Power);            \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Rational);         \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(UnaryFunction);    \
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Variable);

// clang-format off
using ApprovedTypeList = TypeList<
    Addition,
    Constant,
    Float,
    FunctionArgument,
    Integer,
    Matrix,
    Multiplication,
    Power,
    Rational,
    UnaryFunction,
    Variable
    >;
// clang-format on

// Base type for visitors that produce expressions.
class VisitorBase {
 public:
  DECLARE_ALL_VIRTUAL_APPLY_METHODS()
};

// The type of error that is generated when a visitor fails to implement an `Apply` method.
enum class VisitorPolicy {
  // Throw an exception.
  // This is useful you have a visitor that should only ever be evaluated on specific types, but
  // needs to be able to compile w/o defining an operation on every type.
  Throw,
  // Fail at compile time.
  CompileError,
  // Silently do nothing. (Allow non-implemented Apply for some types).
  // This is useful for visitors implemented via Lambda, which operate on one specific type only.
  NoError,
};

}  // namespace math
