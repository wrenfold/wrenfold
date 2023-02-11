// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
#include <string>

#include "expression.h"
#include "template_utils.h"
#include "visitor_base.h"

namespace math {

// Implementation of a visitor.
template <typename Derived>
class VisitorImpl : public VisitorBase {
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
    : public VisitorImpl<VisitorWithCapturedResult<ReturnType, VisitorType, P>> {
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
// If the visitor policy is CompileError or Throw, this method returns `VisitorType::ReturnType`.
// Otherwise, it returns std::optional<VisitorType::ReturnType>. If the required visitor is not
// implemented, the optional will be empty.
template <typename VisitorType>
auto VisitStruct(const Expr& expr, VisitorType&& visitor) {
  // We need this `ReturnType` property because we cannot deduce the result type of operator()
  // w/o first knowing the argument type. The argument type is unknown until the visitor is
  // invoked on a concrete type.
  using ReturnType = typename MaybeVoid<typename std::decay_t<VisitorType>::ReturnType>::Type;
  constexpr VisitorPolicy Policy = std::decay_t<VisitorType>::Policy;
  VisitorWithCapturedResult<ReturnType, VisitorType, Policy> capture_visitor{visitor};
  expr.Receive(capture_visitor);
  if constexpr (Policy == VisitorPolicy::CompileError || Policy == VisitorPolicy::Throw) {
    ASSERT(capture_visitor.result.has_value());
    return std::move(*capture_visitor.result);  // ReturnType
  } else {
    return capture_visitor.result;  // std::optional<ReturnType>
  }
}

// This type exists so that we can deduce a Lambda return type.
// Since the lambda may take auto, we evaluate it with all the approved types. `Apply` is
// implemented only for those types that we can invoke the lambda with successfully.
template <typename Lambda, VisitorPolicy PolicyIn>
struct LambdaWrapper {
  // Deduce the return type of the lambda by invoking it w/ the approved type list.
  using ReturnType = typename CallableReturnType<Lambda, ApprovedTypeList>::Type;

  // Policy is user-specified.
  static constexpr VisitorPolicy Policy = PolicyIn;

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

namespace detail {
// Visit using a lambda. We construct a struct
template <VisitorPolicy Policy, typename F>
auto VisitLambdaWithPolicy(const Expr& u, F&& func) {
  LambdaWrapper<F, Policy> wrapper{std::move(func)};
  return VisitStruct(u, std::move(wrapper));
}
}  // namespace detail

// Try to visit an expression with a lambda or function pointer.
// If the type matches, the lambda will be called and the optional will be filled w/ the result.
template <typename F>
auto VisitLambda(const Expr& u, F&& func) {
  return detail::VisitLambdaWithPolicy<VisitorPolicy::NoError, F>(u, std::forward<F>(func));
}

// Visit two expressions with a struct that accepts two concrete types in its Apply(...) signature.
// The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto VisitBinaryStruct(const Expr& u, const Expr& v, VisitorType&& handler) {
  //  using ReturnType = typename MaybeVoid<typename std::decay_t<VisitorType>::ReturnType>::Type;
  using ReturnType = typename std::decay_t<VisitorType>::ReturnType;
  constexpr VisitorPolicy Policy = std::decay_t<VisitorType>::Policy;

  // If the policy specifies that the visitor is mandatory, we can return the type
  // directly. Otherwise, it will be an optional.
  if constexpr (Policy == VisitorPolicy::CompileError || Policy == VisitorPolicy::Throw) {
    return detail::VisitLambdaWithPolicy<Policy>(u, [&handler, &v](const auto& typed_u) {
      return detail::VisitLambdaWithPolicy<Policy>(v, [&handler, &typed_u](const auto& typed_v) {
        // Check if we can visit
        using TypeU = std::decay_t<decltype(typed_u)>;
        using TypeV = std::decay_t<decltype(typed_v)>;
        static_assert(!std::is_const_v<decltype(handler)>);
        static_assert(HasBinaryApplyMethod<std::decay_t<VisitorType>, TypeU, TypeV>,
                      "Binary visitor fails to implement a required Apply() method.");
        return handler.Apply(typed_u, typed_v);
      });
    });
  } else {
    // Passing the return type out of the nested `Visit` calls proves to be a big pain. Instead,
    // we declare a new result here and ignore the two that are instantiated by each `Visit`.
    std::optional<ReturnType> result{};
    VisitLambda(u, [&handler, &v, &result](const auto& typed_u) {
      using TypeU = std::decay_t<decltype(typed_u)>;
      VisitLambda(v, [&handler, &typed_u, &result](const auto& typed_v) {
        // Check if we can visit
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
}

// Cast to type `T` using a visitor. Returns nullptr if the cast is invalid.
// Based on some experiments w/ quick-bench, this is 6x-9x faster than dynamic_cast.
// TODO: Profile on a more complicated expression tree to see how much difference it makes.
// TODO: This should probably use a custom type-id system.
template <typename T>
const T* TryCast(const Expr& x) {
  return VisitLambda(x, [](const T& y) { return &y; }).value_or(nullptr);
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
template <typename Derived, typename Argument>
void ApplyOrThrow(VisitorImpl<Derived>& visitor, const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    visitor.AsDerived().Apply(arg);
  } else if constexpr (VisitorImpl<Derived>::Policy == VisitorPolicy::Throw) {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
  static_assert(HasApplyMethod<Derived, Argument> ||
                    VisitorImpl<Derived>::Policy != VisitorPolicy::CompileError,
                "The visitor fails to implement a required method");
}

}  // namespace math
