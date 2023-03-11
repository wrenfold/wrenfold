// Copyright 2023 Gareth Cross
#pragma once
#include "ast.h"
#include "expressions/function_argument.h"
#include "function_evaluator_detail.h"
#include "type_annotations.h"

namespace math {

template <typename ReturnType, typename... Args, typename... ArgumentInfo>
std::tuple<FunctionDescription, std::vector<Expr>> BuildFunctionDescription(
    ReturnType (*func)(Args...), const std::string_view function_name,
    const ArgumentInfo&... args_in) {
  static_assert(sizeof...(Args) == sizeof...(ArgumentInfo), "Mismatch in # args and # arg names");
  static_assert(std::conjunction_v<std::is_constructible<Arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");
  using ArgList = TypeList<Args...>;  //  List of all argument types.

  const std::array<Arg, sizeof...(ArgumentInfo)> args = {Arg(args_in)...};

  FunctionDescription desc{};
  desc.function_name = function_name;
  detail::RecordArgs<ArgList>(desc, args, std::make_index_sequence<sizeof...(Args)>());
  if constexpr (!std::is_same_v<void, ReturnType>) {
    detail::RecordReturnTypes<ReturnType>{}(desc);
  }

  // Build inputs and invoke the function
  std::vector<std::unique_ptr<const detail::CapturedOutputBase>> captured_outputs{};
  const auto result = detail::InvokeWithOutputCapture<ArgList>(
      func, captured_outputs, std::make_index_sequence<sizeof...(Args)>());

  std::vector<Expr> outputs;
  for (const auto& ptr : captured_outputs) {
    ptr->GetOutputExpressions(outputs);
  }
  detail::CopyOutputExpressions<std::remove_cv_t<decltype(result)>>{}(result, outputs);
  return std::make_tuple(std::move(desc), std::move(outputs));
}

}  // namespace math
