// Copyright 2023 Gareth Cross
#pragma once
#include "code_generation/ast.h"
#include "expressions/function_argument.h"
#include "function_evaluator_detail.h"
#include "type_annotations.h"

namespace math {

// Invoke the provided function `func` and capture all the output expressions.
// The outputs are inspected and converted into an instance of `ast::FunctionSignature` and a
// vector of output expressions.
template <typename Func, typename... ArgumentInfo>
std::tuple<ast::FunctionSignature, std::vector<ExpressionGroup>> BuildFunctionDescription(
    Func&& func, const std::string_view function_name, ArgumentInfo&&... args_in) {
  static_assert(std::conjunction_v<std::is_constructible<Arg, ArgumentInfo>...>,
                "args_in must be convertible to type Arg.");

  // Extract return type and argument list of the provided function pointer or lambda.
  using Traits = FunctionTraits<Func>;
  using ArgList = typename Traits::ArgsTypeList;  //  List of all argument types.
  using ReturnType = typename Traits::ReturnType;
  static_assert(TypeListSize<ArgList>::Value == sizeof...(ArgumentInfo),
                "Mismatch in # args and # arg names");

  // Convert args into an array so that we can index them.
  const std::array<Arg, sizeof...(ArgumentInfo)> args = {
      Arg(std::forward<ArgumentInfo>(args_in))...};

  // Build inputs and invoke the function
  std::tuple outputs = detail::InvokeWithOutputCapture<ArgList>(
      std::forward<Func>(func), std::make_index_sequence<Traits::Arity>());

  // Copy expressions into `ExpressionGroup` objects, one per output:
  std::vector<ExpressionGroup> groups{};
  detail::CopyOutputExpressionsFromTuple(outputs, groups);

  // Add all the input arguments:
  ast::FunctionSignature signature{std::string(function_name)};
  detail::RecordInputArgs<ArgList>(signature, args, std::make_index_sequence<Traits::Arity>());

  // Record all the output arguments:
  std::apply(
      [&](auto&&... output_expression) {
        static_assert(
            std::conjunction_v<
                IsOutputArgOrReturnValue<std::decay_t<decltype(output_expression)>>...>,
            "All returned elements of the tuple must be explicitly marked as `ReturnValue` or "
            "`OutputArg`.");
        (detail::RecordOutput<std::decay_t<decltype(output_expression)>>{}(signature,
                                                                           output_expression),
         ...);
      },
      outputs);

  return std::make_tuple(std::move(signature), std::move(groups));
}

}  // namespace math
