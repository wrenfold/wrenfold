// Copyright 2022 Gareth Cross
#pragma once
#include <functional>
#include <optional>
#include <tuple>
#include <variant>

namespace math {

// Does nothing but act as a list of types:
template <typename... Ts>
struct TypeList {};

// Struct for getting argument types and return types from a function pointer or lambda.
// This specialization is for lambdas.
template <typename T>
struct FunctionTraits : public FunctionTraits<decltype(&T::operator())> {};

// This specialization is for member functions (like operator() on a lambda).
template <typename ClassType, typename Ret, typename... Args>
struct FunctionTraits<Ret (ClassType::*)(Args...) const> {
  constexpr static auto Arity = sizeof...(Args);

  // Return type of the function.
  using ReturnType = Ret;

  // The arg type list.
  using ArgsTypeList = TypeList<Args...>;

  // Get the i'th argument type.
  template <std::size_t i>
  using ArgType = typename std::tuple_element<i, std::tuple<Args...>>::type;
};

// This specialization is for general function pointers.
template <typename Ret, typename... Args>
struct FunctionTraits<Ret (*)(Args...)> {
  constexpr static auto Arity = sizeof...(Args);

  // Return type of the function.
  using ReturnType = Ret;

  // The arg type list.
  using ArgsTypeList = TypeList<Args...>;

  // Get the i'th argument type.
  template <std::size_t i>
  using ArgType = typename std::tuple_element<i, std::tuple<Args...>>::type;
};

// Template to check if the `operator()` method is implemented.
template <typename T, typename, typename = void>
constexpr bool HasCallOperator = false;

// Specialization that is activated when the Apply method exists:
template <typename T, typename Argument>
constexpr bool HasCallOperator<
    T, Argument, decltype(std::declval<T>()(std::declval<const Argument>()), void())> = true;

// Template to check if the `operator()` method is implemented for two types.
template <typename T, typename, typename, typename = void>
constexpr bool HasBinaryCallOperator = false;

// Specialization that is activated when the binary operator() method exists.
template <typename T, typename Argument1, typename Argument2>
constexpr bool HasBinaryCallOperator<T, Argument1, Argument2,
                                     decltype(std::declval<T>()(std::declval<const Argument1>(),
                                                                std::declval<const Argument2>()),
                                              void())> = true;

// Size of type list.
template <typename T>
struct TypeListSize;
template <typename... Ts>
struct TypeListSize<TypeList<Ts...>> {
  static constexpr std::size_t Value = sizeof...(Ts);
};

// Check if a type is in a TypeList.
template <typename T, typename... Ts>
constexpr bool ContainsTypeHelper = std::disjunction_v<std::is_same<T, Ts>...>;
template <typename T, typename U>
constexpr bool ContainsType = false;
template <typename T, typename... Ts>
constexpr bool ContainsType<T, TypeList<Ts...>> = ContainsTypeHelper<T, Ts...>;

// Helper to append a type to the front of a type list.
template <typename, typename>
struct AppendToTypeList;
template <typename T, typename... Args>
struct AppendToTypeList<T, TypeList<Args...>> {
  using Type = TypeList<T, Args...>;
};

// Ge the head of a type list.
template <typename T>
struct HeadOfTypeList {};
template <typename Head, typename... Ts>
struct HeadOfTypeList<TypeList<Head, Ts...>> {
  using Type = Head;
};

// Get the index of a type in a type list.
template <typename T, typename U = void, typename... Types>
constexpr std::size_t IndexOfTypeHelper() {
  return std::is_same<T, U>::value ? 0 : 1 + IndexOfTypeHelper<T, Types...>();
}
template <typename T, typename List>
struct IndexOfType;
template <typename T, typename... Ts>
struct IndexOfType<T, TypeList<Ts...>> {
  constexpr static std::size_t Value = IndexOfTypeHelper<T, Ts...>();
};

// Get the N'th element of a type list.
template <std::size_t N, typename... Ts>
struct TypeListElement;

template <std::size_t N, typename T, typename... Ts>
struct TypeListElement<N, TypeList<T, Ts...>> {
  using Type = typename TypeListElement<N - 1, TypeList<Ts...>>::Type;
};
template <typename T, typename... Ts>
struct TypeListElement<0, TypeList<T, Ts...>> {
  using Type = T;
};

// This template iterates over a TypeList and creates a new TypeList of return types that occur
// when `Callable` is invoked with each type in the input type list. Duplicates may occur.
template <typename Callable, typename...>
struct CallOperatorReturnTypesImpl;

template <typename Callable>
struct CallOperatorReturnTypesImpl<Callable> {
  using Type = TypeList<>;
};

template <typename Callable, typename Head, typename... Tail>
struct CallOperatorReturnTypesImpl<Callable, Head, Tail...> {
  using CandidateType =
      typename std::conditional_t<HasCallOperator<Callable, Head>,
                                  std::invoke_result_t<Callable, Head>, std::nullptr_t>;

  // Don't append nullptr_t, this is the signal for an invocation that isn't valid (no operator()).
  using Type = typename std::conditional_t<
      !std::is_same_v<CandidateType, std::nullptr_t>,
      typename AppendToTypeList<
          CandidateType, typename CallOperatorReturnTypesImpl<Callable, Tail...>::Type>::Type,
      typename CallOperatorReturnTypesImpl<Callable, Tail...>::Type>;
};

// Simplified interface for invoking CallOperatorReturnTypesImpl.
template <typename Callable, typename T>
struct CallOperatorReturnTypes;
template <typename Callable, typename... Ts>
struct CallOperatorReturnTypes<Callable, TypeList<Ts...>> {
  // Build the list of possible turn types.
  using List = typename CallOperatorReturnTypesImpl<Callable, Ts...>::Type;
  // Get the first one (TODO: Check they all match?)
  using Head = typename HeadOfTypeList<List>::Type;
};

// Select `Indices` elements from a tuple. Returns a new tuple with just those elements.
template <typename Tuple, std::size_t... Indices>
auto SelectFromTuple(Tuple&& tuple, std::index_sequence<Indices...>) {
  return std::tuple<std::tuple_element_t<Indices, std::remove_reference_t<Tuple>>...>(
      std::get<Indices>(std::forward<Tuple>(tuple))...);
}

// If `T` is an r-value reference, get the decayed type.
// Otherwise, get a const T&.
template <typename T>
struct DecayRValueToValue {
  using Type =
      std::conditional_t<std::is_rvalue_reference_v<T>, std::decay_t<T>, const std::decay_t<T>&>;
};

// True if the type is a tuple.
template <typename T>
constexpr bool IsTuple = false;
template <typename... Ts>
constexpr bool IsTuple<std::tuple<Ts...>> = true;

namespace detail {
template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

// A user-defined deduction guide for `Overloaded`.
template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;
}  // namespace detail

// Create a struct w/ multiple operator() methods (determined by `funcs`).
template <typename... Funcs>
auto MakeOverloaded(Funcs&&... funcs) {
  return detail::Overloaded{std::forward<Funcs>(funcs)...};
}

// Visit a variant w/ the lambdas `funcs`. Whichever lambda matches the variant type is invoked.
// If the last lambda is `auto`, it will match any remaining types.
template <typename... Funcs, typename Variant>
auto OverloadedVisit(Variant&& var, Funcs&&... funcs) {
  return std::visit(MakeOverloaded(std::forward<Funcs>(funcs)...), std::forward<Variant>(var));
}

}  // namespace math