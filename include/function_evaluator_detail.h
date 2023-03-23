// Copyright 2023 Gareth Cross
#pragma once
#include "expressions/function_argument.h"
#include "function_description.h"
#include "template_utils.h"
#include "type_annotations.h"

namespace math {
namespace detail {

template <typename T>
struct CopyOutputExpressions {
  void operator()(const T&, std::vector<Expr>&) const {
    throw TypeError("Unhandled output expression type");
  }
};

template <>
struct CopyOutputExpressions<Expr> {
  void operator()(const Expr& val, std::vector<Expr>& outputs) const {
    ASSERT(!GetUnfilledExprPlaceholder().IsIdenticalTo(val),
           "One of the output expressions was not filled during function evaluation.");
    outputs.push_back(val);
  }
};

template <index_t Rows, index_t Cols>
struct CopyOutputExpressions<type_annotations::StaticMatrix<Rows, Cols>> {
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
struct CopyOutputExpressions<std::tuple<Ts...>> {
  template <std::size_t... Indices>
  void operator()(const std::tuple<Ts...>& tup, std::vector<Expr>& outputs,
                  std::index_sequence<Indices...>) const {
    using Tuple = std::tuple<Ts...>;
    (CopyOutputExpressions<std::tuple_element_t<Indices, Tuple>>{}(std::get<Indices>(tup), outputs),
     ...);
  }

  void operator()(const std::tuple<Ts...>& tup, std::vector<Expr>& outputs) const {
    this->operator()(tup, outputs, std::make_index_sequence<sizeof...(Ts)>());
  }
};

template <typename T, bool OutputArg>
struct RecordFunctionArgument;

template <bool OutputArg>
struct RecordFunctionArgument<Expr, OutputArg> {
  void operator()(FunctionDescription& desc, const Arg& arg) const {
    if (OutputArg) {
      desc.AddOutput(arg.GetName(), ast::ScalarType(), arg.IsOptional());
    } else {
      desc.AddInput(arg.GetName(), ast::ScalarType(), false);
    }
  }
};

template <index_t Rows, index_t Cols, bool OutputArg>
struct RecordFunctionArgument<type_annotations::StaticMatrix<Rows, Cols>, OutputArg> {
  void operator()(FunctionDescription& desc, const Arg& arg) const {
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
void RecordArgsImpl(FunctionDescription& desc, const std::array<Arg, N>& args) {
  static_assert(Index < N);
  using ArgType = typename TypeListElement<Index, ArgList>::Type;
  RecordFunctionArgument<std::decay_t<ArgType>, IsOutputArgument<ArgType>>{}(desc, args[Index]);
}

template <typename ArgList, std::size_t... Indices, std::size_t N>
void RecordArgs(FunctionDescription& desc, const std::array<Arg, N>& args,
                std::index_sequence<Indices...>) {
  (RecordArgsImpl<ArgList, Indices>(desc, args), ...);
}

template <typename T>
struct RecordReturnTypes;

template <>
struct RecordReturnTypes<Expr> {
  void operator()(FunctionDescription& desc) const { desc.AddReturnValue(ast::ScalarType()); }
};

template <index_t Rows, index_t Cols>
struct RecordReturnTypes<type_annotations::StaticMatrix<Rows, Cols>> {
  void operator()(FunctionDescription& desc) const {
    desc.AddReturnValue(ast::MatrixType(Rows, Cols));
  }
};

template <typename... Args>
struct RecordReturnTypes<std::tuple<Args...>> {
  void operator()(FunctionDescription& desc) const { (RecordReturnTypes<Args>()(desc), ...); }
};

// We can't default initialize `Expr`, since it cannot be null.
// This object is just to assign a placeholder value to outputs that will be written when
// a user symbolic functions is called.
template <typename T>
struct InitializeStorage;
template <>
struct InitializeStorage<Expr> {
  Expr operator()() const { return GetUnfilledExprPlaceholder(); }
};
template <index_t Rows, index_t Cols>
struct InitializeStorage<type_annotations::StaticMatrix<Rows, Cols>> {
  type_annotations::StaticMatrix<Rows, Cols> operator()() const {
    std::vector<Expr> expressions(static_cast<std::size_t>(Rows * Cols),
                                  GetUnfilledExprPlaceholder());
    MatrixExpr expr = MatrixExpr::Create(Rows, Cols, std::move(expressions));
    return type_annotations::StaticMatrix<Rows, Cols>(std::move(expr));
  }
};

// Captured output is used to store output arguments that are filled by a user
// provided function. We instantiate one of these for every output argument,
// and then copy the output values by calling `GetOutputExpressions`.
class CapturedOutputBase {
 public:
  virtual ~CapturedOutputBase() = default;
  virtual void GetOutputExpressions(std::vector<Expr>& outputs) const = 0;
};

template <typename T>
class CapturedOutput final : public CapturedOutputBase {
 public:
  CapturedOutput() : storage_(InitializeStorage<T>{}()) {}

  T& GetMutableRef() { return storage_; }

  // Copy from our stored type `T` to the output.
  void GetOutputExpressions(std::vector<Expr>& outputs) const override {
    CopyOutputExpressions<T>{}(storage_, outputs);
  }

 private:
  T storage_{};
};

template <typename T>
struct BuildFunctionArgument;

template <>
struct BuildFunctionArgument<Expr> {
  auto operator()(std::size_t arg_index) const { return FunctionArgument::Create(arg_index, 0); }
};

template <index_t Rows, index_t Cols>
struct BuildFunctionArgument<type_annotations::StaticMatrix<Rows, Cols>> {
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

template <typename T, std::size_t Index>
std::conditional_t<IsOutputArgument<T>, std::decay_t<T>&, std::decay_t<T>> BuildFunctionArguments(
    std::size_t& input_index, std::vector<std::unique_ptr<const CapturedOutputBase>>& outputs) {
  using TDecay = std::decay_t<T>;
  if constexpr (IsOutputArgument<T>) {
    auto ptr = std::make_unique<CapturedOutput<TDecay>>();
    TDecay& result = ptr->GetMutableRef();
    outputs.push_back(std::move(ptr));
    return result;
  } else {
    return BuildFunctionArgument<TDecay>{}(input_index++);
  }
}

template <typename ArgList, typename Callable, std::size_t... Indices>
auto InvokeWithOutputCapture(Callable callable,
                             std::vector<std::unique_ptr<const CapturedOutputBase>>& outputs,
                             std::index_sequence<Indices...>) {
  std::size_t input_index = 0;
  return std::invoke(
      callable, BuildFunctionArguments<typename TypeListElement<Indices, ArgList>::Type, Indices>(
                    input_index, outputs)...);
}

}  // namespace detail
}  // namespace math
