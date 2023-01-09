// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
#include <string>

#include "expression.h"
#include "template_utils.h"
#include "visitor_base.h"

namespace math {

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
  VisitorWithCapturedResult<ReturnType, VisitorType, Policy> capture_visitor{visitor};
  expr.Receive(capture_visitor);
  return capture_visitor.result;  //  std::optional<ReturnType>
}

// This type exists so that we can deduce a Lambda return type.
// Since the lambda may take auto, we evaluate it with all the approved types. `Apply` is
// implemented only for those types that we can invoke the lambda with successfully.
template <typename Lambda>
struct LambdaWrapper {
  // Deduce the return type of the lambda by invoking it w/ the approved type list.
  using ReturnType = typename CallableReturnType<Lambda, ApprovedTypeList>::Type;

  // Lambdas will typically either:
  // - Accept one type only, in which case any other error policy makes no sense.
  // - Accept `auto` and switch on the type internally. Because this means the lambda body
  // can be instantiated with any type (or an error occurs), we can't detect if a given type
  // is supported with HasApplyMethod<>.
  static constexpr VisitorPolicy Policy = VisitorPolicy::NoError;

  // Construct by moving lambda inside.
  explicit LambdaWrapper(Lambda&& func) : func_(std::move(func)) {}

  // If the lambda implements an operator() that accepts `Argument`, we declare an `Apply()`
  // method that calls it.
  template <typename Argument>
  std::enable_if_t<HasCallOperator<Lambda, Argument>, ReturnType> Apply(const Argument& arg) {
    return func_(arg);
  }

 protected:
  Lambda func_;
};

// Try to visit an expression with a lambda or function pointer.
// If the type matches, the lambda will be called and the optional will be filled w/ the result.
template <typename F>
auto VisitLambda(const Expr& u, F&& func) {
  // This wrapper allows a lambda that accepts `auto` to be passed to `VisitStruct`.
  LambdaWrapper wrapper{std::move(func)};
  return VisitStruct(u, wrapper);
}

// Visit two expressions with a struct that accepts two concrete types in its Apply(...) signature.
// The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto VisitBinaryStruct(const Expr& u, const Expr& v, VisitorType&& handler) {
  using ReturnType = typename MaybeVoid<typename std::decay_t<VisitorType>::ReturnType>::Type;
  // Passing the return type out of the nested `Visit` calls proves to be a big pain. Instead,
  // we declare a new result here and ignore the two that are instantiated by each `Visit`.
  // TODO: Are they actually optimized out?
  std::optional<ReturnType> result{};
  VisitLambda(u, [&handler, &v, &result](const auto& typed_u) -> void {
    VisitLambda(v, [&handler, &typed_u, &result](const auto& typed_v) -> void {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(!std::is_const_v<decltype(handler)>);
      if constexpr (HasBinaryApplyMethod<std::decay_t<VisitorType>, TypeU, TypeV>) {
        if constexpr (!std::is_same_v<ReturnType, Void>) {
          result = handler.Apply(typed_u, typed_v);
        } else {
          handler.Apply(typed_u, typed_v);
          result = Void{};
        }
      }
    });
  });
  return result;
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
