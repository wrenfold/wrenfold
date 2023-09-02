// Copyright 2023 Gareth Cross
#pragma once
#include "code_generation/ast.h"
#include "expressions/function_argument.h"
#include "function_evaluator_detail.h"
#include "type_annotations.h"

namespace math {

template <typename T, typename F, std::size_t... I>
constexpr auto CreateVectorFromSequence(F&& func, std::integer_sequence<T, I...>) {
  using U = std::invoke_result_t<F, T>;
  std::vector<U> result;
  result.reserve(sizeof...(I));
  (result.push_back(func(I)), ...);
  return result;
}

template <typename T>
constexpr bool IsTuple = false;

template <typename... Ts>
constexpr bool IsTuple<std::tuple<Ts...>> = true;

template <typename T>
constexpr auto MaybeMakeTuple(T&& arg) {
  if constexpr (IsTuple<T>) {
    // Just copy it.
    return T{std::forward<T>(arg)};
  } else {
    return std::make_tuple(std::forward<T>(arg));
  }
}

template <typename Func, typename... ArgumentInfo>
std::tuple<ast::FunctionSignature, std::vector<ExpressionGroup>> BuildFunctionDescription(
    Func&& func, const std::string_view function_name, const ArgumentInfo&... args_in) {
  static_assert(std::conjunction_v<std::is_constructible<Arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");

  // Extract return type and argument list of the provided function pointer or lambda.
  using Traits = FunctionTraits<Func>;
  using ArgList = typename Traits::ArgsTypeList;  //  List of all argument types.
  using ReturnType = typename Traits::ReturnType;
  static_assert(TypeListSize<ArgList>::Value == sizeof...(ArgumentInfo),
                "Mismatch in # args and # arg names");

  const std::array<Arg, sizeof...(ArgumentInfo)> args = {Arg(args_in)...};

  ast::FunctionSignature signature{std::string(function_name)};
  detail::RecordArgs<ArgList>(signature, args, std::make_index_sequence<Traits::Arity>());
  if constexpr (!std::is_same_v<void, ReturnType>) {
    detail::RecordReturnTypes<ReturnType>{}(signature);
  }

  // Build inputs and invoke the function
  auto [result, output_args] = detail::InvokeWithOutputCapture<ArgList>(
      std::forward<Func>(func), std::make_index_sequence<Traits::Arity>());

  // If result was not a tuple, we convert it to a tuple w/ a single element.
  const std::tuple result_as_tuple = MaybeMakeTuple(std::move(result));

  // Get the indices of only output arguments, as an array.
  // Stupid that we have to use vector here, but std::array cannot be default constructed when size
  // is zero.
  const std::vector<OutputKey> output_arg_keys = CreateVectorFromSequence(
      [&](std::size_t pos) {
        return OutputKey(args[pos].IsOptional() ? ExpressionUsage::OptionalOutputArgument
                                                : ExpressionUsage::OutputArgument,
                         pos);
      },
      detail::FilterArguments<false>(ArgList{}));

  // Compile time iota for the # of return values:
  constexpr std::size_t num_return_vals = std::tuple_size_v<decltype(result_as_tuple)>;
  const std::vector<OutputKey> return_val_keys = CreateVectorFromSequence(
      [&](std::size_t pos) { return OutputKey(ExpressionUsage::ReturnValue, pos); },
      std::make_index_sequence<num_return_vals>());

  std::vector<ExpressionGroup> groups;
  groups.reserve(output_arg_keys.size() + return_val_keys.size());

  detail::CopyOutputExpressions(output_args, output_arg_keys, groups);
  detail::CopyOutputExpressions(result_as_tuple, return_val_keys, groups);

  return std::make_tuple(std::move(signature), std::move(groups));
}

}  // namespace math
