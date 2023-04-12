// Copyright 2023 Gareth Cross
#pragma once

#include "ast.h"
#include "constants.h"
#include "expressions/function_argument.h"
#include "template_utils.h"
#include "type_annotations.h"

namespace math {
namespace detail {

template <typename T>
struct CopyOutputExpressionsImpl {};

template <>
struct CopyOutputExpressionsImpl<Expr> {
  void operator()(const Expr& val, std::vector<Expr>& outputs) const {
    ASSERT(!GetUnfilledExprPlaceholder().IsIdenticalTo(val),
           "One of the output expressions was not filled during function evaluation.");
    outputs.push_back(val);
  }
};

template <index_t Rows, index_t Cols>
struct CopyOutputExpressionsImpl<type_annotations::StaticMatrix<Rows, Cols>> {
  void operator()(const type_annotations::StaticMatrix<Rows, Cols>& val,
                  std::vector<Expr>& outputs) const {
    for (index_t row = 0; row < Rows; ++row) {
      for (index_t col = 0; col < Cols; ++col) {
        ASSERT(!GetUnfilledExprPlaceholder().IsIdenticalTo(val(row, col)),
               "One of the output expressions was not filled during function evaluation. Row = {}, "
               "Col = {}",
               row, col);
        outputs.push_back(val(row, col));
      }
    }
  }
};

template <typename... Ts>
struct CopyOutputExpressionsImpl<std::tuple<Ts...>> {
  template <std::size_t... Indices>
  void operator()(const std::tuple<Ts...>& tup, std::vector<Expr>& outputs,
                  std::index_sequence<Indices...>) const {
    using Tuple = std::tuple<Ts...>;
    (CopyOutputExpressionsImpl<std::tuple_element_t<Indices, Tuple>>{}(std::get<Indices>(tup),
                                                                       outputs),
     ...);
  }

  void operator()(const std::tuple<Ts...>& tup, std::vector<Expr>& outputs) const {
    this->operator()(tup, outputs, std::make_index_sequence<sizeof...(Ts)>());
  }
};

template <typename T>
void CopyOutputExpressions(const T& arg, std::vector<Expr>& outputs) {
  CopyOutputExpressionsImpl<T>{}(arg, outputs);
}

template <typename T, bool OutputArg>
struct RecordFunctionArgument;

template <bool OutputArg>
struct RecordFunctionArgument<Expr, OutputArg> {
  void operator()(ast::FunctionSignature& desc, const Arg& arg) const {
    if (OutputArg) {
      desc.AddOutput(arg.GetName(), ast::ScalarType(), arg.IsOptional());
    } else {
      desc.AddInput(arg.GetName(), ast::ScalarType(), false);
    }
  }
};

template <index_t Rows, index_t Cols, bool OutputArg>
struct RecordFunctionArgument<type_annotations::StaticMatrix<Rows, Cols>, OutputArg> {
  void operator()(ast::FunctionSignature& desc, const Arg& arg) const {
    if (OutputArg) {
      desc.AddOutput(arg.GetName(), ast::MatrixType(Rows, Cols), arg.IsOptional());
    } else {
      desc.AddInput(arg.GetName(), ast::MatrixType(Rows, Cols), false);
    }
  }
};

// Non-const references are output arguments.
template <typename T>
constexpr bool IsOutputArgument =
    std::is_reference_v<T> && !std::is_const_v<std::remove_reference_t<T>>;

template <typename ArgList, std::size_t Index, std::size_t N>
void RecordArgsImpl(ast::FunctionSignature& desc, const std::array<Arg, N>& args) {
  static_assert(Index < N);
  using ArgType = typename TypeListElement<Index, ArgList>::Type;
  RecordFunctionArgument<std::decay_t<ArgType>, IsOutputArgument<ArgType>>{}(desc, args[Index]);
}

template <typename ArgList, std::size_t... Indices, std::size_t N>
void RecordArgs(ast::FunctionSignature& desc, const std::array<Arg, N>& args,
                std::index_sequence<Indices...>) {
  (RecordArgsImpl<ArgList, Indices>(desc, args), ...);
}

template <typename T>
struct RecordReturnTypes;

template <>
struct RecordReturnTypes<Expr> {
  void operator()(ast::FunctionSignature& desc) const { desc.AddReturnValue(ast::ScalarType()); }
};

template <index_t Rows, index_t Cols>
struct RecordReturnTypes<type_annotations::StaticMatrix<Rows, Cols>> {
  void operator()(ast::FunctionSignature& desc) const {
    desc.AddReturnValue(ast::MatrixType(Rows, Cols));
  }
};

template <typename... Args>
struct RecordReturnTypes<std::tuple<Args...>> {
  void operator()(ast::FunctionSignature& desc) const { (RecordReturnTypes<Args>()(desc), ...); }
};

template <typename T, bool IsOutputArg>
struct BuildFunctionArgumentImpl;

template <bool IsOutputArg>
struct BuildFunctionArgumentImpl<Expr, IsOutputArg> {
  auto operator()(std::size_t arg_index) const {
    if constexpr (IsOutputArg) {
      return GetUnfilledExprPlaceholder();
    } else {
      return FunctionArgument::Create(arg_index, 0);
    }
  }
};

template <index_t Rows, index_t Cols, bool IsOutputArg>
struct BuildFunctionArgumentImpl<type_annotations::StaticMatrix<Rows, Cols>, IsOutputArg> {
  auto operator()(std::size_t arg_index) const {
    std::vector<Expr> expressions{};
    expressions.reserve(static_cast<std::size_t>(Rows * Cols));
    for (std::size_t i = 0; i < Rows * Cols; ++i) {
      if constexpr (IsOutputArg) {
        (void)arg_index;  //  Suppress unused variable warning on GCC.
        expressions.push_back(GetUnfilledExprPlaceholder());
      } else {
        expressions.push_back(FunctionArgument::Create(arg_index, i));
      }
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
  return BuildFunctionArgumentImpl<TDecay, IsOutputArgument<T>>{}(Index);
}

template <std::size_t... A, std::size_t... B>
constexpr std::index_sequence<A..., B...> operator+(std::index_sequence<A...>,
                                                    std::index_sequence<B...>) {
  return {};
}

template <std::size_t Index, typename... Args>
constexpr bool IndexIsOutputArg =
    detail::IsOutputArgument<typename TypeListElement<Index, TypeList<Args...>>::Type>;

// Create two index_sequence objects. The first contains the indices of input arguments, while
// the second contains indices of output arguments.
template <bool TakeInputs, typename... Args, std::size_t... Indices>
constexpr auto FilterArguments(std::index_sequence<Indices...>) {
  return (std::conditional_t<!IndexIsOutputArg<Indices, Args...> == TakeInputs,
                             std::index_sequence<Indices>, std::index_sequence<>>() +
          ...);
}

template <bool TakeInputs, typename... Args>
constexpr auto FilterArguments() {
  return FilterArguments<TakeInputs, Args...>(std::make_index_sequence<sizeof...(Args)>());
}

template <bool TakeInputs, typename... Args>
constexpr auto FilterArguments(TypeList<Args...>) {
  return FilterArguments<TakeInputs, Args...>();
}

template <typename ArgList, typename Callable, std::size_t... Indices>
auto InvokeWithOutputCapture(Callable callable, std::index_sequence<Indices...>) {
  static_assert(sizeof...(Indices) <= TypeListSize<ArgList>::Value);

  // Create a tuple of arguments. Inputs are created as `FunctionArgument` objects, while outputs
  // are unfilled place-holders (since we cannot default-initialize Expr) that `callable` will
  // replace.
  auto args = std::make_tuple(BuildFunctionArguments<Indices, ArgList>()...);

  // Call the user provided function with the args we just created:
  auto return_values = std::invoke(std::move(callable), std::get<Indices>(args)...);

  // Now extract the output arguments:
  constexpr auto output_arg_indices = FilterArguments<false>(ArgList{});
  auto output_arguments = SelectFromTuple(args, output_arg_indices);

  return std::make_pair(std::move(return_values), std::move(output_arguments));
}

// tests:
static_assert(!IsOutputArgument<int>);
static_assert(!IsOutputArgument<const int&>);
static_assert(IsOutputArgument<int&>);
static_assert(!IndexIsOutputArg<0, int, const double&, double&>);
static_assert(!IndexIsOutputArg<1, int, const double&, double&>);
static_assert(IndexIsOutputArg<2, int, const double&, double&>);
static_assert(std::is_same_v<std::index_sequence<1, 2>,
                             decltype(FilterArguments<false>(TypeList<int, int&, double&>{}))>);

}  // namespace detail
}  // namespace math
