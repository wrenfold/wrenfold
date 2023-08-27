// Copyright 2022 Gareth Cross
#pragma once
#include <optional>
#include <string>

#include "assertions.h"
#include "expression.h"
#include "template_utils.h"
#include "visitor_base.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4250)  // inherit via dominance
#endif

namespace math {

// Implement abstract method from `VisitorDeclare`.
template <typename Derived, typename T>
class VisitorImpl : public virtual VisitorDeclare<T> {
 public:
  void ApplyVirtual(const T& arg) override {
    // Check if derived type implements an apply method for T.
    if constexpr (HasApplyMethod<Derived, T>) {
      static_cast<Derived*>(this)->Apply(arg);
    }
    static_assert(HasApplyMethod<Derived, T>, "The visitor fails to implement a required method");
  }
};

// Inherit from `VisitorImpl` for all types in a type list.
template <typename Derived, typename T>
class VisitorImplAll;
template <typename Derived, typename... Ts>
class VisitorImplAll<Derived, TypeList<Ts...>> : public VisitorBaseGeneric<TypeList<Ts...>>,
                                                 public VisitorImpl<Derived, Ts>... {};

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
template <typename ReturnType, typename VisitorType, typename Types>
struct VisitorWithCapturedResultGeneric final
    : public VisitorImplAll<VisitorWithCapturedResultGeneric<ReturnType, VisitorType, Types>,
                            Types> {
 public:
  // Construct with non-const ref to visitor type.
  VisitorWithCapturedResultGeneric(VisitorType& impl) : impl_(impl) {}

  // Call the implementation. This method is enabled only if the visitor
  // has an `Apply` method that accepts type `Argument`. This is required so that the visitor
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
  using ReturnType = typename std::decay_t<VisitorType>::ReturnType;
  using ReturnTypeOrVoid = typename MaybeVoid<ReturnType>::Type;

  VisitorWithCapturedResultGeneric<ReturnTypeOrVoid, VisitorType, ApprovedTypeList> capture_visitor{
      visitor};
  expr.Receive(static_cast<VisitorBase&>(capture_visitor));
  ASSERT(capture_visitor.result.has_value());
  return std::move(*capture_visitor.result);  // ReturnType
}

namespace detail {

// Create a tuple that will capture `args` so it can be moved into a lambda.
// - R-value references are converted to values and moved.
// - Values will be copied.
// - Other references will be passed by reference into the tuple.
template <typename... CapturedArgs>
auto MakeArgCaptureTuple(CapturedArgs&&... args) {
  return std::tuple<std::conditional_t<std::is_rvalue_reference_v<CapturedArgs>,
                                       std::decay_t<CapturedArgs>, CapturedArgs>...>{
      std::forward<CapturedArgs>(args)...};
}

static_assert(std::is_same_v<std::tuple<int, std::string>,
                             decltype(MakeArgCaptureTuple(5, std::string("test")))>);
static_assert(std::is_same_v<std::tuple<int, const float&>,
                             decltype(MakeArgCaptureTuple(5, std::declval<const float&>()))>);

// This type exists so that we can deduce a Lambda return type.
// Since the lambda may take auto, we evaluate it with all the approved types. `Apply` is
// implemented only for those types that we can invoke the lambda with successfully.
template <typename Lambda>
struct LambdaWrapper {
  // Deduce the return type of the lambda by invoking it w/ the approved type list.
  using ReturnType = typename CallableReturnType<Lambda, ApprovedTypeList>::Type;

  // ConstructMatrix by moving lambda inside.
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
}  // namespace detail

// Visit an expression with a lambda or function pointer.
template <typename F>
auto VisitLambda(const Expr& u, F&& func) {
  detail::LambdaWrapper<F> wrapper{std::forward<F>(func)};
  return VisitStruct(u, std::move(wrapper));
}

template <typename VisitorType, typename... CapturedArgs>
auto VisitStruct(const Expr& expr, VisitorType&& visitor, CapturedArgs&&... captured_args) {
  // Capture the arguments in a tuple (r-values args are moved into values).
  // Based on: https://stackoverflow.com/questions/63414770
  auto arg_tuple = detail::MakeArgCaptureTuple(std::forward<CapturedArgs>(captured_args)...);

  return VisitLambda(expr,
                     [&visitor, arg_tuple = std::move(arg_tuple)](const auto& concrete) mutable {
                       return std::apply(
                           [&visitor, &concrete](auto&&... args) {
                             return visitor.Apply(concrete, std::forward<decltype(args)>(args)...);
                           },
                           std::move(arg_tuple));
                     });
}

// Visit two expressions with a struct that accepts two concrete types in its Apply(...) signature.
// The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto VisitBinaryStruct(const Expr& u, const Expr& v, VisitorType&& handler) {
  return VisitLambda(u, [&handler, &v](const auto& typed_u) {
    return VisitLambda(v, [&handler, &typed_u](const auto& typed_v) {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(!std::is_const_v<decltype(handler)>);
      static_assert(HasBinaryApplyMethod<std::decay_t<VisitorType>, TypeU, TypeV>,
                    "Binary visitor fails to implement a required Apply() method.");
      return handler.Apply(typed_u, typed_v);
    });
  });
}

}  // namespace math

#ifdef _MSC_VER
#pragma warning(pop)
#endif
