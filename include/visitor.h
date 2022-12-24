// Copyright 2022 Gareth Cross
#pragma once
#include <string>

#include "expression_fwd.h"
#include "operations_fwd.h"

namespace math {

// Declare pure virtual Apply method.
// These are non-const, since visitors may need to cache or update internal values.
#define DECLARE_VIRTUAL_APPLY_METHOD(ArgType) \
  virtual ExpressionBaseConstPtr ApplyVirtual(const ArgType& arg) = 0

// Implement override of the base method by calling the correct version for `ArgType`:
#define IMPLEMENT_VIRTUAL_APPLY_METHOD(ArgType)                      \
  ExpressionBaseConstPtr ApplyVirtual(const ArgType& arg) override { \
    return ApplyOrThrow(*this, arg);                                 \
  }

class VisitorWithResultBase {
 public:
  DECLARE_VIRTUAL_APPLY_METHOD(Addition);
  DECLARE_VIRTUAL_APPLY_METHOD(Constant);
  DECLARE_VIRTUAL_APPLY_METHOD(Division);
  DECLARE_VIRTUAL_APPLY_METHOD(Multiplication);
  DECLARE_VIRTUAL_APPLY_METHOD(NaturalLog);
  DECLARE_VIRTUAL_APPLY_METHOD(Negation);
  DECLARE_VIRTUAL_APPLY_METHOD(Number);
  DECLARE_VIRTUAL_APPLY_METHOD(Power);
  DECLARE_VIRTUAL_APPLY_METHOD(Subtraction);
  DECLARE_VIRTUAL_APPLY_METHOD(Variable);
};

// Template to check if the `Apply` method is implemented.
template <typename T, typename, typename = void>
constexpr bool HasApplyMethod = false;

// Specialization that is activated when the Apply method exists:
template <typename T, typename Argument>
constexpr bool HasApplyMethod<
    T, Argument, decltype(std::declval<T>().Apply(std::declval<const Argument>()), void())> = true;

template <typename Derived>
class VisitorWithResultImpl : public VisitorWithResultBase {
 public:
  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_VIRTUAL_APPLY_METHOD(Addition);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Constant);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Division);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Multiplication);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(NaturalLog);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Negation);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Number);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Power);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Subtraction);
  IMPLEMENT_VIRTUAL_APPLY_METHOD(Variable);
};

// Thrown for un-implemented visitors.
class VisitorNotImplemented final : public std::exception {
 public:
  // Construct w/ the visitor name and the argument name.
  VisitorNotImplemented(const char* VisitorName, const char* ArgumentName);

  // Return string for the exception.
  const char* what() const noexcept override;

 private:
  std::string what_;
};

// Dispatch call using static polymorphism, or throw if not implemented yet for `Argument`.
template <typename Derived, typename Argument>
ExpressionBaseConstPtr ApplyOrThrow(VisitorWithResultImpl<Derived>& visitor, const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    return visitor.AsDerived().Apply(arg);
  } else {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
}

}  // namespace math
