// Copyright 2023 Gareth Cross
#pragma once
#include "wf/assertions.h"
#include "wf/matrix_functions.h"

namespace wf {

// Store information about an argument to a function.
// User specifies this as an argument to `build_function_description`.
struct arg {
  // Construct with string name of the argument.
  explicit arg(const std::string_view name) : name_(name) {}

  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

namespace type_annotations {

// A wrapper around `MatrixExpr` with statically declared dimensions.
// This is so that we can write functions in C++, and have the argument dimensions accessible at
// compile time.
template <index_t Rows, index_t Cols>
struct static_matrix {
  // Allow implicit construction from MatrixExpr.
  static_matrix(MatrixExpr expr) : expr_(std::move(expr)) {  // NOLINT(google-explicit-constructor)
    WF_ASSERT_EQUAL(Rows, expr_.rows());
    WF_ASSERT_EQUAL(Cols, expr_.cols());
  }

  constexpr index_t rows() const noexcept { return Rows; }
  constexpr index_t cols() const noexcept { return Cols; }

  template <typename... Args>
  using enable_if_convertible_to_expr =
      std::enable_if_t<std::conjunction_v<std::is_constructible<Expr, Args>...>>;

  // Construct from a row-major orderd list of elements.
  template <typename... Args, typename = enable_if_convertible_to_expr<Args...>>
  explicit static_matrix(Args&&... args)
      : static_matrix(make_matrix(Rows, Cols, std::forward<Args>(args)...)) {
    static_assert(sizeof...(args) == Rows * Cols,
                  "Wrong # of elements passed to matrix constructor.");
  }

  const Expr& operator()(index_t row, index_t col) const { return expr_(row, col); }

  // Access vector element.
  const Expr& operator[](index_t element) const { return expr_[element]; }

  // Assign from MatrixExpr
  static_matrix& operator=(const MatrixExpr& other) {
    WF_ASSERT_EQUAL(Rows, other.rows());
    WF_ASSERT_EQUAL(Cols, other.cols());
    expr_ = other;
    return *this;
  }

  // Explicit cast to MatrixExpr.
  constexpr const MatrixExpr& inner() const noexcept { return expr_; }

  // Implicit cast to MatrixExpr.
  constexpr operator const MatrixExpr&() const noexcept { return expr_; }  // NOLINT

  // Transpose:
  [[nodiscard]] static_matrix<Cols, Rows> transposed() const {
    return static_matrix<Cols, Rows>{expr_.transposed()};
  }

 private:
  MatrixExpr expr_;
};

// Evaluates to true if `T` is a static_matrix type annotation.
template <typename>
struct is_static_matrix : std::false_type {};
template <index_t Rows, index_t Cols>
struct is_static_matrix<static_matrix<Rows, Cols>> : std::true_type {};
template <typename T>
constexpr bool is_static_matrix_v = is_static_matrix<T>::value;

}  // namespace type_annotations
}  // namespace wf
