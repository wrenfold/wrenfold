// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
#include <string>

#include "expression.h"
#include "visitor_base.h"

namespace math {

// Template to check if the `Apply` method is implemented.
template <typename T, typename, typename = void>
constexpr bool HasApplyMethod = false;

// Specialization that is activated when the Apply method exists:
template <typename T, typename Argument>
constexpr bool HasApplyMethod<
    T, Argument, decltype(std::declval<T>().Apply(std::declval<const Argument>()), void())> = true;

// Implementation of a visitor that returns an expression.
template <typename Derived>
class VisitorWithResultImpl : public VisitorWithResultBase {
 public:
  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()
};

// Implementation of a visitor that does not return an expression.
// Visitors with void return type may optionally choose to avoid throwing
// when the return value is unspecified.
template <typename Derived, bool Throwing = true>
class VisitorWithoutResultImpl : public VisitorWithoutResultBase {
 public:
  // Cast to non-const derived type.
  Derived& AsDerived() { return static_cast<Derived&>(*this); }

  IMPLEMENT_ALL_VIRTUAL_APPLY_METHODS()
};

// Wraps a type-erased visitor. The wrapped visitor must produce `ReturnType`
// as the result of a call to Apply(...). If no visitor method can be called,
// the result is left empty.
template <typename ReturnType, typename VisitorType>
struct TypeErasedVisitor final
    : public VisitorWithoutResultImpl<TypeErasedVisitor<ReturnType, VisitorType>, false> {
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
template <typename VisitorType>
struct TypeErasedVisitor<void, VisitorType> final
    : public VisitorWithoutResultImpl<TypeErasedVisitor<void, VisitorType>, false> {
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
// Returns `std::optional<VisitorType::ReturnType>`. If the visitor does not
// implement a method that applies to the underlying type of `Expr`, the optional
// will be left empty.
template <typename VisitorType>
auto Visit(const Expr& expr, VisitorType&& visitor) {
  // We need this `ReturnType` property because it is not possible to reduce
  // the lambda return type w/o knowing the argument type. We require the user
  // to specify explicitly for clarity.
  using ReturnType = typename std::decay_t<VisitorType>::ReturnType;
  if constexpr (std::is_lvalue_reference_v<VisitorType>) {
    // If visitor is an l-value reference, use a pointer as the type for the type-erased visitor.
    // This pointer remains valid for the duration of this method.
    TypeErasedVisitor<ReturnType, std::decay_t<VisitorType>*> erased_visitor{&visitor};
    expr.Receive(erased_visitor);
    if constexpr (!std::is_same_v<ReturnType, void>) {
      return erased_visitor.result;
    }
  } else {
    TypeErasedVisitor<ReturnType, VisitorType> erased_visitor{std::forward<VisitorType>(visitor)};
    expr.Receive(erased_visitor);
    if constexpr (!std::is_same_v<ReturnType, void>) {
      return erased_visitor.result;
    }
  }
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
template <typename Derived, bool Throwing, typename Argument>
void ApplyOrThrow(VisitorWithoutResultImpl<Derived, Throwing>& visitor, const Argument& arg) {
  if constexpr (HasApplyMethod<Derived, Argument>) {
    visitor.AsDerived().Apply(arg);
  } else if (Throwing) {
    throw VisitorNotImplemented(typeid(Derived).name(), typeid(Argument).name());
  }
}

}  // namespace math
