// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
#include <string>

#include "expression.h"
#include "function_traits.h"
#include "visitor_base.h"

namespace math {

// Template to check if the `Apply` method is implemented.
template <typename T, typename, typename = void>
constexpr bool HasApplyMethod = false;

// Specialization that is activated when the Apply method exists:
template <typename T, typename Argument>
constexpr bool HasApplyMethod<
    T, Argument, decltype(std::declval<T>().Apply(std::declval<const Argument>()), void())> = true;

// Implementation of a visitor.
template <typename Derived, typename ResultType>
class VisitorImpl : public VisitorBase<ResultType> {
 public:
  static constexpr VisitorPolicy Policy = Derived::Policy;

  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()
};

// Some amazing magic. TODO: Make language feature.
struct Void {};

// MaybeVoid converts `void` -> `Void`, so that we can put something in an optional<>
template <typename T>
struct MaybeVoid {
  using Type = T;
};
template <>
struct MaybeVoid<void> {
  using Type = Void;
};

// Wraps a type-erased visitor. The wrapped visitor must produce `ReturnType`
// as the result of a call to Apply(...). If no visitor method can be called,
// the result is left empty.
template <typename ReturnType, typename VisitorType, VisitorPolicy P>
struct VisitorWithCapturedResult final
    : public VisitorImpl<VisitorWithCapturedResult<ReturnType, VisitorType, P>, void> {
 public:
  static constexpr VisitorPolicy Policy = P;

  // Construct with non-const ref to visitor type.
  VisitorWithCapturedResult(VisitorType& impl) : impl_(impl) {}

  // Call the implementation. This method is enabled only if the visitor
  // has an `Apply` method that accepts type `Argument`. This is required so that the visitor policy
  // correctly fails to compile when the user neglects to implement a type.
  template <typename Argument>
  std::enable_if_t<HasApplyMethod<VisitorType, Argument>, void> Apply(const Argument& arg) {
    if constexpr (!std::is_same_v<ReturnType, Void>) {
      result = impl_.Apply(arg);
    } else {
      impl_.Apply(arg);
      result = Void{};  //  Set the optional, so we record that a visitor ran.
    }
  }

 private:
  VisitorType& impl_;

 public:
  std::optional<ReturnType> result{};
};

// Accepts a visitor struct and applies it to the provided expression.
// Returns `std::optional<VisitorType::ReturnType>`. If the visitor does not implement the required
// method for a given expression type, the outcome is determined by VisitorType::Policy.
template <typename VisitorType>
auto VisitStruct(const Expr& expr, VisitorType&& visitor) {
  // We need this `ReturnType` property because we cannot deduce the result type of operator()
  // w/o first knowing the argument type. The argument type is unknown until the visitor is
  // invoked on a concrete type.
  using ReturnType = typename MaybeVoid<typename std::decay_t<VisitorType>::ReturnType>::Type;
  constexpr VisitorPolicy Policy = std::decay_t<VisitorType>::Policy;
  VisitorWithCapturedResult<ReturnType, VisitorType, Policy> erased_visitor{visitor};
  expr.Receive(erased_visitor);
  return erased_visitor.result;  //  std::optional<ReturnType>
}

// Struct that wraps user specified lambda.
// This exists so that we can use VisitStruct, but with a lambda. The lambda must be converted to
// a struct, so that
template <typename Lambda>
struct TemporaryStruct {
  using ReturnType = typename function_traits<std::decay_t<Lambda>>::ReturnType;
  using Argument = typename function_traits<std::decay_t<Lambda>>::template DecayedArgType<0>;
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  // Construct from lambda.
  explicit TemporaryStruct(Lambda func) : func_(std::move(func)) {}

  // Call the underlying lambda.
  ReturnType Apply(const Argument& typed_u) { return func_(typed_u); }

 private:
  Lambda func_;
};

// Try to visit an expression with a lambda or function pointer.
// If the type matches, the lambda will be called and the optional will be filled w/ the result.
template <typename F>
auto VisitLambda(const Expr& u, F&& func) {
  TemporaryStruct<F> visitor{std::move(func)};
  return VisitStruct(u, visitor);
}

// Version of `VisitLambda` that visits two expressions simultaneously.
template <typename F>
auto VisitLambda(const Expr& u, const Expr& v, F&& func) {
  using traits = function_traits<std::decay_t<F>>;
  static_assert(traits::Arity == 2, "Must be a function of two arguments");
  using TypeU = typename traits::template DecayedArgType<0>;
  using TypeV = typename traits::template DecayedArgType<1>;
  return VisitLambda(u, [func = std::move(func), &v](const TypeU& typed_u) {
    return VisitLambda(v, [func = std::move(func), &typed_u](const TypeV& typed_v) {
      return func(typed_u, typed_v);
    });
  });
}

// Cast to type `T` using a visitor. Returns nullptr if the cast is invalid.
// Based on some experiments w/ quick-bench, this is 6x-9x faster than dynamic_cast.
// TODO: Profile on a more complicated expression tree to see how much difference it makes.
template <typename T>
const T* TryCast(const Expr& x) {
  return VisitLambda(x, [](const T& y) { return &y; }).value_or(nullptr);
}

// Visitor that checks if the underlying type is `T`.
template <typename T>
bool IsType(const Expr& x) {
  return VisitLambda(x, [](const T&) constexpr {}).has_value();
}

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

// Variant of ApplyOrThrow that takes no argument.
template <typename Derived, typename ReturnType, typename Argument>
ReturnType ApplyOrThrow(VisitorImpl<Derived, ReturnType>& visitor, const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    return visitor.AsDerived().Apply(arg);
  } else if constexpr (VisitorImpl<Derived, ReturnType>::Policy == VisitorPolicy::Throw) {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
  static_assert(HasApplyMethod<Derived, Argument> ||
                    VisitorImpl<Derived, ReturnType>::Policy != VisitorPolicy::CompileError,
                "The visitor fails to implement a required method");
}

}  // namespace math
