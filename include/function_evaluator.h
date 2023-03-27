// Copyright 2023 Gareth Cross
#pragma once
#include "ast.h"
#include "expressions/function_argument.h"
#include "function_evaluator_detail.h"
#include "type_annotations.h"

namespace math {

template <typename ReturnType, typename... Args, typename... ArgumentInfo>
std::tuple<ast::FunctionSignature, std::vector<Expr>> BuildFunctionDescription(
    ReturnType (*func)(Args...), const std::string_view function_name,
    const ArgumentInfo&... args_in) {
  static_assert(sizeof...(Args) == sizeof...(ArgumentInfo), "Mismatch in # args and # arg names");
  static_assert(std::conjunction_v<std::is_constructible<Arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");
  using ArgList = TypeList<Args...>;  //  List of all argument types.

  const std::array<Arg, sizeof...(ArgumentInfo)> args = {Arg(args_in)...};

  ast::FunctionSignature signature{std::string(function_name)};
  detail::RecordArgs<ArgList>(signature, args, std::make_index_sequence<sizeof...(Args)>());
  if constexpr (!std::is_same_v<void, ReturnType>) {
    detail::RecordReturnTypes<ReturnType>{}(signature);
  }

  // Build inputs and invoke the function
  const auto [result, output_args] =
      detail::InvokeWithOutputCapture<ArgList>(func, std::make_index_sequence<sizeof...(Args)>());

  std::vector<Expr> outputs;
  detail::CopyOutputExpressions(output_args, outputs);
  detail::CopyOutputExpressions(result, outputs);
  return std::make_tuple(std::move(signature), std::move(outputs));
}

}  // namespace math
