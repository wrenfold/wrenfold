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
    if constexpr (HasCallOperator<Derived, T>) {
      static_cast<Derived&>(*this)(arg);
    }
    static_assert(HasCallOperator<Derived, T>, "The visitor fails to implement a required method");
  }
};

// Inherit from `VisitorImpl` for all types in a type list.
template <typename Derived, typename T>
class VisitorImplAll;
template <typename Derived, typename... Ts>
class VisitorImplAll<Derived, TypeList<Ts...>> : public VisitorBaseGeneric<TypeList<Ts...>>,
                                                 public VisitorImpl<Derived, Ts>... {};

namespace detail {

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
// as the result of a call to operator()(...). If no visitor method can be called,
// the result is left empty.
template <typename ReturnType, typename VisitorType, typename Types>
struct VisitorWithCapturedResult final
    : public VisitorImplAll<VisitorWithCapturedResult<ReturnType, VisitorType, Types>, Types> {
 public:
  // Construct with non-const ref to visitor type.
  VisitorWithCapturedResult(VisitorType& impl) : impl_(impl) {}

  // Call the implementation. This method is enabled only if the visitor
  // has an `operator()` method that accepts type `Argument`. This is required so that the visitor
  // correctly fails to compile when the user neglects to implement a type.
  template <typename Argument>
  std::enable_if_t<HasCallOperator<VisitorType, Argument>, void> operator()(const Argument& arg) {
    if constexpr (!std::is_same_v<ReturnType, Void>) {
      result = impl_(arg);
    } else {
      impl_(arg);
      result = Void{};
    }
  }

  // Move the result out of the visitor and return it.
  ReturnType TakeResult() {
    if constexpr (std::is_default_constructible_v<ReturnType>) {
      return std::move(result);
    } else {
      ASSERT(result.has_value());
      return std::move(*result);
    }
  }

 private:
  VisitorType& impl_;

 public:
  std::conditional_t<std::is_default_constructible_v<ReturnType>, ReturnType,
                     std::optional<ReturnType>>
      result{};
};

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
}  // namespace detail

// Accepts a visitor struct or lambda and applies it to the provided expression.
// If the visitor policy is CompileError or Throw, this method returns `VisitorType::ReturnType`.
// Otherwise, it returns std::optional<VisitorType::ReturnType>. If the required visitor is not
// implemented, the optional will be empty.
template <typename VisitorType>
auto Visit(const Expr& expr, VisitorType&& visitor) {
  // Deduce the return type by invoking the operator() w/ the different expression types.
  // TODO: For now we allow one single ReturnType. We could allow returning std::variant<>.
  using ReturnType = typename CallOperatorReturnTypes<VisitorType, ApprovedTypeList>::Head;
  using ReturnTypeOrVoid = typename detail::MaybeVoid<ReturnType>::Type;

  detail::VisitorWithCapturedResult<ReturnTypeOrVoid, VisitorType, ApprovedTypeList>
      capture_visitor{visitor};
  expr.Receive(static_cast<VisitorBase&>(capture_visitor));
  if constexpr (!std::is_same_v<ReturnTypeOrVoid, detail::Void>) {
    return capture_visitor.TakeResult();
  }
}

// Visit w/ captured arguments. The operator() on the visitor is expected to take the concrete
// type contained in `expr`, plus all the forwarded args from `captured_args`.
template <typename VisitorType, typename... CapturedArgs>
auto Visit(const Expr& expr, VisitorType&& visitor, CapturedArgs&&... captured_args) {
  // Capture the arguments in a tuple (r-values args are moved into values).
  // Based on: https://stackoverflow.com/questions/63414770
  auto arg_tuple = detail::MakeArgCaptureTuple(std::forward<CapturedArgs>(captured_args)...);
  return Visit(expr, [&visitor, arg_tuple = std::move(arg_tuple)](const auto& concrete) mutable {
    return std::apply(
        [&visitor, &concrete](auto&&... args) {
          return visitor(concrete, std::forward<decltype(args)>(args)...);
        },
        std::move(arg_tuple));
  });
}

// Visit two expressions with a struct that accepts two concrete types in its operator()(...)
// signature. The struct must declare a ReturnType associated type.
template <typename VisitorType>
auto VisitBinaryStruct(const Expr& u, const Expr& v, VisitorType&& handler) {
  return Visit(u, [&handler, &v](const auto& typed_u) {
    return Visit(v, [&handler, &typed_u](const auto& typed_v) {
      // Check if we can visit
      using TypeU = std::decay_t<decltype(typed_u)>;
      using TypeV = std::decay_t<decltype(typed_v)>;
      static_assert(!std::is_const_v<decltype(handler)>);
      static_assert(HasBinaryCallOperator<std::decay_t<VisitorType>, TypeU, TypeV>,
                    "Binary visitor fails to implement a required operator()() method.");
      return handler(typed_u, typed_v);
    });
  });
}

}  // namespace math

#ifdef _MSC_VER
#pragma warning(pop)
#endif
