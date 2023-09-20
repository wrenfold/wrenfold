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
using ExpressionTypeList = TypeList<
    class Addition,
//    class BinaryFunction,
    class Conditional,
    class Constant,
    class Float,
    class FunctionArgument,
    class Infinity,
    class Integer,
    class Matrix,
    class Multiplication,
    class Power,
    class Rational,
    class Relational,
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
template <typename Types>
class VisitorBaseGeneric : public VisitorDeclareAll<Types> {
 public:
  virtual ~VisitorBaseGeneric() = default;
};

using VisitorBase = VisitorBaseGeneric<ExpressionTypeList>;

}  // namespace math
