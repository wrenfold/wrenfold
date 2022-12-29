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

// Template to check if `operator()` exists and accepts `Argument`.
template <typename T, typename, typename = void>
constexpr bool HasCallOperator = false;

// Template to check if `operator()` exists and accepts `Argument`.
template <typename T, typename Argument>
constexpr bool HasCallOperator<
    T, Argument, decltype(std::declval<T>()(std::declval<const Argument>()), void())> = true;

// Implementation of a visitor that returns an expression.
template <typename Derived>
class VisitorWithResultImpl : public VisitorWithResultBase {
 public:
  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()
};

// The type of error that is generated when a visitor fails to implement
// an `Apply` method for a type.
enum class VisitorPolicy {
  // Throw an exception.
  Throw,
  // Fail at compile time.
  CompileError,
  // Silently do nothing. (Allow non-implemented Apply for some types).
  NoError,
};

// Implementation of a visitor that does not return an expression.
// Visitors with void return type may optionally choose to avoid throwing
// when the return value is unspecified.
template <typename Derived, VisitorPolicy Policy = VisitorPolicy::Throw>
class VisitorWithoutResultImpl : public VisitorWithoutResultBase {
 public:
  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()
};

// Wraps a type-erased visitor. The wrapped visitor must produce `ReturnType`
// as the result of a call to Apply(...). If no visitor method can be called,
// the result is left empty.
template <typename ReturnType, typename VisitorType, VisitorPolicy Policy>
struct TypeErasedVisitor final
    : public VisitorWithoutResultImpl<TypeErasedVisitor<ReturnType, VisitorType, Policy>, Policy> {
 public:
  // Move construct from visitor type.
  TypeErasedVisitor(VisitorType&& impl) : impl_(std::move(impl)) {}

  // Call the implementation. This method is enabled only if the visitor
  // has an `Apply` method that accepts type `Argument`.
  template <typename Argument>
  std::enable_if_t<!std::is_pointer_v<VisitorType> && HasApplyMethod<VisitorType, Argument>> Apply(
      const Argument& arg) {
    result = impl_.Apply(arg);
  }

  // Call the implementation. This method is enabled if the VisitorType is a pointer.
  template <typename Argument>
  std::enable_if_t<std::is_pointer_v<VisitorType> &&
                   HasApplyMethod<std::decay_t<VisitorType>, Argument>>
  Apply(const Argument& arg) {
    result = impl_->Apply(arg);
  }

 private:
  VisitorType impl_;

 public:
  std::optional<ReturnType> result{};
};

// Specialization for void ReturnType.
template <typename VisitorType, VisitorPolicy Policy>
struct TypeErasedVisitor<void, VisitorType, Policy> final
    : public VisitorWithoutResultImpl<TypeErasedVisitor<void, VisitorType, Policy>, Policy> {
 public:
  // Move construct from visitor type.
  TypeErasedVisitor(VisitorType&& impl) : impl_(std::move(impl)) {}

  // Call the implementation. This method is enabled only if the visitor
  // has an `Apply` method that accepts type `Argument`.
  template <typename Argument>
  std::enable_if_t<!std::is_pointer_v<VisitorType> && HasApplyMethod<VisitorType, Argument>> Apply(
      const Argument& arg) {
    impl_.Apply(arg);
  }

  // Call the implementation. This method is enabled only if the visitor
  // has an `operator()` method that accepts type `Argument`.
  template <typename Argument>
  std::enable_if_t<!std::is_pointer_v<VisitorType> && HasCallOperator<VisitorType, Argument>> Apply(
      const Argument& arg) {
    impl_(arg);
  }

  // Call the implementation. This method is enabled if the VisitorType is a pointer.
  template <typename Argument>
  std::enable_if_t<std::is_pointer_v<VisitorType> &&
                   HasApplyMethod<std::remove_pointer_t<VisitorType>, Argument>>
  Apply(const Argument& arg) {
    impl_->Apply(arg);
  }

 private:
  VisitorType impl_;
};

// Accepts a visitor struct and applies it to the provided expression.
// Returns `std::optional<VisitorType::ReturnType>`. If the visitor does not implement the required
// method for a given expression type, the outcome is determined by VisitorType::Policy.
template <typename VisitorType>
auto Visit(const Expr& expr, VisitorType&& visitor) {
  // We need this `ReturnType` property because it is not possible to reduce
  // the lambda return type w/o knowing the argument type. We require the user
  // to specify explicitly for clarity.
  using ReturnType = typename std::decay_t<VisitorType>::ReturnType;
  constexpr VisitorPolicy Policy = std::decay_t<VisitorType>::Policy;
  if constexpr (std::is_lvalue_reference_v<VisitorType>) {
    // If visitor is an l-value reference, use a pointer as the type for the type-erased visitor.
    // This pointer remains valid for the duration of this method.
    TypeErasedVisitor<ReturnType, std::decay_t<VisitorType>*, Policy> erased_visitor{&visitor};
    expr.Receive(erased_visitor);
    if constexpr (!std::is_same_v<ReturnType, void>) {
      return erased_visitor.result;
    }
  } else {
    // Otherwise the visitor is a value or a forwarded reference.
    // In that case, forward it and `TypeErasedVisitor` will own the resulting value.
    TypeErasedVisitor<ReturnType, VisitorType, Policy> erased_visitor{
        std::forward<VisitorType>(visitor)};
    expr.Receive(erased_visitor);
    if constexpr (!std::is_same_v<ReturnType, void>) {
      return erased_visitor.result;
    }
  }
}

// Make a type-erased visitor from a lambda or function pointer.
template <typename F>
auto MakeTypeErasedVisitor(F&& func) {
  return TypeErasedVisitor<void, std::decay_t<F>, VisitorPolicy::NoError>{std::forward<F>(func)};
}

// Try to visit an expression with a lambda or function pointer.
// If the type matches, the lambda will be called and the optional will be filled w/ the result.
template <typename F>
auto TryVisit(const Expr& u, F&& func) {
  using traits = function_traits<std::decay_t<F>>;
  static_assert(traits::Arity == 1, "Must be a unary function");
  using TypeU = typename traits::template DecayedArgType<0>;

  // Based on some inspection on quick-bench, this is about ~6-9x faster than doing a dynamic_cast
  // to operate on a specific subtype.
  std::optional<typename traits::ReturnType> result;
  auto visitor_u = MakeTypeErasedVisitor(
      [func = std::move(func), &result](const TypeU& typed_u) { result = func(typed_u); });
  u.Receive(visitor_u);
  return result;
}

template <typename F>
auto TryVisit(const Expr& u, const Expr& v, F&& func) {
  using traits = function_traits<std::decay_t<F>>;
  static_assert(traits::Arity == 2, "Must be a function of two arguments");
  using TypeU = typename traits::template DecayedArgType<0>;
  using TypeV = typename traits::template DecayedArgType<1>;

  std::optional<typename traits::ReturnType> result;
  auto visitor_u =
      MakeTypeErasedVisitor([func = std::move(func), &v, &result](const TypeU& typed_u) {
        auto visitor_v =
            MakeTypeErasedVisitor([func = std::move(func), &typed_u, &result](
                                      const TypeV& typed_v) { result = func(typed_u, typed_v); });
        v.Receive(visitor_v);
      });
  u.Receive(visitor_u);
  return result;
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

// Dispatch call using static polymorphism, or throw if not implemented yet for `Argument`.
template <typename Derived, typename Argument>
ExpressionConceptConstPtr ApplyOrThrow(VisitorWithResultImpl<Derived>& visitor,
                                       const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    // TODO: Make sure this is a move. Expr is being destroyed anyways.
    Expr expr = visitor.AsDerived().Apply(arg);
    return expr.GetImpl();
  } else {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
}

// Variant of ApplyOrThrow that takes no argument.
template <typename Derived, VisitorPolicy Policy, typename Argument>
void ApplyOrThrow(VisitorWithoutResultImpl<Derived, Policy>& visitor, const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    visitor.AsDerived().Apply(arg);
  } else if constexpr (Policy == VisitorPolicy::Throw) {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
  static_assert(HasApplyMethod<Derived, Argument> || Policy != VisitorPolicy::CompileError,
                "The visitor fails to implement a required method");
}

}  // namespace math
