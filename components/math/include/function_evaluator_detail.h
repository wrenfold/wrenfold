// Copyright 2023 Gareth Cross
#pragma once

#include "code_generation/ast.h"
#include "code_generation/expression_group.h"
#include "constants.h"
#include "expressions/function_argument.h"
#include "output_annotations.h"
#include "template_utils.h"
#include "type_annotations.h"

namespace math {
namespace detail {

template <typename T>
struct CopyOutputExpressionsImpl;

template <>
struct CopyOutputExpressionsImpl<Expr> {
  void operator()(const Expr& val, std::vector<Expr>& outputs) const { outputs.push_back(val); }
};

template <>
struct CopyOutputExpressionsImpl<MatrixExpr> {
  void operator()(const MatrixExpr& val, std::vector<Expr>& outputs) const {
    outputs.reserve(val.Size());
    for (index_t row = 0; row < val.NumRows(); ++row) {
      for (index_t col = 0; col < val.NumCols(); ++col) {
        outputs.push_back(val(row, col));
      }
    }
  }
};

template <index_t Rows, index_t Cols>
struct CopyOutputExpressionsImpl<type_annotations::StaticMatrix<Rows, Cols>> {
  void operator()(const type_annotations::StaticMatrix<Rows, Cols>& val,
                  std::vector<Expr>& outputs) const {
    CopyOutputExpressionsImpl<MatrixExpr>{}.operator()(val, outputs);
  }
};

template <typename T>
ExpressionGroup CreateExpressionGroup(const T& tuple_element) {
  std::vector<Expr> expressions;

  // TInner is the inner type of `ReturnValue` or `OutputArg`.
  using TInner = std::decay_t<decltype(tuple_element.Value())>;
  CopyOutputExpressionsImpl<TInner>{}(tuple_element.Value(), expressions);

  if constexpr (IsReturnValue<T>::value) {
    OutputKey key{ExpressionUsage::ReturnValue, ""};
    return ExpressionGroup(std::move(expressions), std::move(key));
  } else {
    OutputKey key{tuple_element.IsOptional() ? ExpressionUsage::OptionalOutputArgument
                                             : ExpressionUsage::OutputArgument,
                  tuple_element.Name()};
    return ExpressionGroup(std::move(expressions), std::move(key));
  }
}

template <typename... Ts, std::size_t... Indices>
void CopyOutputExpressionsFromTuple(const std::tuple<Ts...>& output_tuple,
                                    std::vector<ExpressionGroup>& groups,
                                    std::index_sequence<Indices...>) {
  static_assert(std::conjunction_v<IsOutputArgOrReturnValue<Ts>...>,
                "All returned elements of the tuple must be explicitly marked as `ReturnValue` or "
                "`OutputArg`.");
  static_assert(CountReturnValues<Ts...> <= 1, "Only one return value is allowed.");
  groups.reserve(sizeof...(Ts));
  // Comma operator ensures the order of evaluation here will be left -> right.
  (groups.push_back(CreateExpressionGroup(std::get<Indices>(output_tuple))), ...);
}

// Create an index sequence, so we can invoke `CopyOutputExpressionsFromTuple` over all the elements
// of the tuple.
template <typename... Ts>
void CopyOutputExpressionsFromTuple(const std::tuple<Ts...>& output_tuple,
                                    std::vector<ExpressionGroup>& groups) {
  CopyOutputExpressionsFromTuple(output_tuple, groups, std::make_index_sequence<sizeof...(Ts)>());
}

template <typename T>
struct RecordOutput;

template <typename T>
struct RecordOutput<ReturnValue<T>> {
  void operator()(ast::FunctionSignature& desc, const ReturnValue<T>& output) const {
    // This is a return value.
    if constexpr (std::is_same_v<Expr, T>) {
      desc.return_value = ast::ScalarType(NumericType::Real);
    } else {
      desc.return_value = ast::MatrixType(output.Value().NumRows(), output.Value().NumCols());
    }
  }
};

template <typename T>
struct RecordOutput<OutputArg<T>> {
  void operator()(ast::FunctionSignature& desc, const OutputArg<T>& output) const {
    if constexpr (std::is_same_v<Expr, T>) {
      desc.AddArgument(output.Name(), ast::ScalarType(NumericType::Real),
                       output.IsOptional() ? ast::ArgumentDirection::OptionalOutput
                                           : ast::ArgumentDirection::Output);
    } else {
      // todo: static assert this is StaticMatrix
      desc.AddArgument(output.Name(),
                       ast::MatrixType(output.Value().NumRows(), output.Value().NumCols()),
                       output.IsOptional() ? ast::ArgumentDirection::OptionalOutput
                                           : ast::ArgumentDirection::Output);
    }
  }
};

template <typename T>
struct RecordInputArgument;

template <>
struct RecordInputArgument<Expr> {
  void operator()(ast::FunctionSignature& desc, const Arg& arg) const {
    desc.AddArgument(arg.GetName(), ast::ScalarType(NumericType::Real),
                     ast::ArgumentDirection::Input);
  }
};

template <index_t Rows, index_t Cols>
struct RecordInputArgument<type_annotations::StaticMatrix<Rows, Cols>> {
  void operator()(ast::FunctionSignature& desc, const Arg& arg) const {
    desc.AddArgument(arg.GetName(), ast::MatrixType(Rows, Cols), ast::ArgumentDirection::Input);
  }
};

template <typename ArgList, std::size_t... Indices, std::size_t N>
void RecordInputArgs(ast::FunctionSignature& desc, const std::array<Arg, N>& args,
                     std::index_sequence<Indices...>) {
  (RecordInputArgument<std::decay_t<typename TypeListElement<Indices, ArgList>::Type>>{}(
       desc, args[Indices]),
   ...);
}

template <typename T>
struct BuildFunctionArgumentImpl;

template <>
struct BuildFunctionArgumentImpl<Expr> {
  auto operator()(std::size_t arg_index) const { return FunctionArgument::Create(arg_index, 0); }
};

template <index_t Rows, index_t Cols>
struct BuildFunctionArgumentImpl<type_annotations::StaticMatrix<Rows, Cols>> {
  auto operator()(std::size_t arg_index) const {
    std::vector<Expr> expressions{};
    expressions.reserve(static_cast<std::size_t>(Rows * Cols));
    for (std::size_t i = 0; i < Rows * Cols; ++i) {
      expressions.push_back(FunctionArgument::Create(arg_index, i));
    }
    MatrixExpr expr = MatrixExpr::Create(Rows, Cols, std::move(expressions));
    return type_annotations::StaticMatrix<Rows, Cols>(std::move(expr));
  }
};

template <std::size_t Index, typename ArgList>
auto BuildFunctionArguments() {
  using T = typename TypeListElement<Index, ArgList>::Type;
  using TDecay = std::decay_t<T>;
  // Increase input_index every time we hit an input argument.
  return BuildFunctionArgumentImpl<TDecay>{}(Index);
}

template <std::size_t... A, std::size_t... B>
constexpr std::index_sequence<A..., B...> operator+(std::index_sequence<A...>,
                                                    std::index_sequence<B...>) {
  return {};
}

// Return an index_sequence<> that contains the indices of just `OutputArg` objects.
// This is used to filter out any return values.
template <typename TupleType, std::size_t... Indices>
constexpr auto SelectOutputArgIndices(std::index_sequence<Indices...>) {
  return (std::conditional_t<IsOutputArg<std::tuple_element_t<Indices, TupleType>>::value,
                             std::index_sequence<Indices>, std::index_sequence<>>() +
          ...);
}
template <typename... Ts>
constexpr auto SelectOutputArgIndices(const std::tuple<Ts...>&) {
  return SelectOutputArgIndices<std::tuple<Ts...>>(std::make_index_sequence<sizeof...(Ts)>());
}

// Invoke the provided callable and capture the output expressions. First builds a tuple of input
// arguments by constructing `FunctionArgument` expressions for every input arg of `callable`. The
// resulting expressions are returned as a tuple of `OutputArg<>` or `ReturnValue<>`.
template <typename ArgList, typename Callable, std::size_t... Indices>
auto InvokeWithOutputCapture(Callable&& callable, std::index_sequence<Indices...>) {
  static_assert(sizeof...(Indices) <= TypeListSize<ArgList>::Value);
  // Create a tuple of arguments. Inputs are created as `FunctionArgument` objects, while outputs
  // are unfilled place-holders (since we cannot default-initialize Expr) that `callable` will
  // replace.
  auto args = std::make_tuple(BuildFunctionArguments<Indices, ArgList>()...);

  // Call the user provided function with the args we just created:
  using ReturnType =
      std::invoke_result_t<Callable, decltype(std::get<Indices>(std::move(args)))...>;
  static_assert(!std::is_same_v<ReturnType, void>, "Return type should not be void.");

  if constexpr (IsTuple<ReturnType>) {
    return std::invoke(std::forward<Callable>(callable), std::get<Indices>(std::move(args))...);
  } else {
    // Function returns a single expression, so convert to a tuple.
    auto return_expr =
        std::invoke(std::forward<Callable>(callable), std::get<Indices>(std::move(args))...);

    if constexpr (IsReturnValue<decltype(return_expr)>::value) {
      return std::make_tuple(std::move(return_expr));
    } else {
      return std::make_tuple(ReturnValue(std::move(return_expr)));
    }
  }
}

}  // namespace detail
}  // namespace math
