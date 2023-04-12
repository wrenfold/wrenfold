// Copyright 2022 Gareth Cross
#pragma once
#include <memory>

#include "enumerations.h"
#include "template_utils.h"

namespace math {

class Expr;
class ExpressionConcept;

// TODO: Allow switching this out for something w/o atomic operations?
using ExpressionConceptConstPtr = std::shared_ptr<const class ExpressionConcept>;

// clang-format off
using ApprovedTypeList = TypeList<
    class Addition,
    class Constant,
    class Float,
    class FunctionArgument,
    class Infinity,
    class Integer,
    class Matrix,
    class Multiplication,
    class Power,
    class Rational,
    class UnaryFunction,
    class Variable
    >;
// clang-format on

// Declare a virtual `Apply` method for the specified type.
template <typename T>
class VisitorDeclare {
 public:
  virtual void ApplyVirtual(const T& input) = 0;
};

// Implements `VisitorDeclare` for every type in a TypeList.
// We use virtual inheritance since both VisitorBase and VisitorImplAll must inherit
// from this, creating a diamond pattern.
template <typename T>
class VisitorDeclareAll;
template <typename... Ts>
class VisitorDeclareAll<TypeList<Ts...>> : public virtual VisitorDeclare<Ts>... {};

// Base type for visitors that produce expressions.
class VisitorBase : public VisitorDeclareAll<ApprovedTypeList> {
 public:
  virtual ~VisitorBase() = default;
};

// The type of error that is generated when a visitor fails to implement an `Apply` method.
namespace VisitorPolicy {
// Fail at compile time.
struct CompileError {};
// Silently do nothing. (Allow non-implemented Apply for some types).
// This is useful for visitors implemented via Lambda, which operate on one specific type only.
struct NoError {};
}  // namespace VisitorPolicy

}  // namespace math
