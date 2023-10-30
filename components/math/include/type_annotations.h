// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "matrix_functions.h"

namespace math {

// Store information about an argument to a function.
// User specifies this as an argument to `build_function_description`.
struct Arg {
  Arg() = default;
  explicit Arg(const std::string_view name) : name_(name) {}

  constexpr const std::string& name() const { return name_; }

 private:
  std::string name_;
};

namespace type_annotations {

template <index_t Rows, index_t Cols>
struct StaticMatrix {
  // Allow implicit construction from MatrixExpr.
  StaticMatrix(MatrixExpr expr) : expr_(std::move(expr)) {  // NOLINT(google-explicit-constructor)
    ZEN_ASSERT_EQUAL(Rows, expr_.rows());
    ZEN_ASSERT_EQUAL(Cols, expr_.cols());
  }

  constexpr index_t rows() const noexcept { return Rows; }
  constexpr index_t cols() const noexcept { return Cols; }

  template <typename... Args>
  static std::enable_if_t<std::conjunction_v<std::is_constructible<Expr, Args>...>, StaticMatrix>
  create(Args&&... args) {
    static_assert(sizeof...(args) == Rows * Cols);
    return StaticMatrix(make_matrix(Rows, Cols, std::move(args)...));
  }

  const Expr& operator()(index_t row, index_t col) const { return expr_(row, col); }

  // Access vector element.
  const Expr& operator[](index_t element) const { return expr_[element]; }

  // Assign from MatrixExpr
  StaticMatrix& operator=(const MatrixExpr& other) {
    ZEN_ASSERT_EQUAL(Rows, other.rows());
    ZEN_ASSERT_EQUAL(Cols, other.cols());
    expr_ = other;
    return *this;
  }

  // Explicit cast to MatrixExpr.
  constexpr const MatrixExpr& inner() const noexcept { return expr_; }

  // Implicit cast to MatrixExpr.
  constexpr operator const MatrixExpr&() const noexcept { return expr_; }  // NOLINT

  // Transpose:
  [[nodiscard]] StaticMatrix<Cols, Rows> transposed() const {
    return StaticMatrix<Cols, Rows>{expr_.transposed()};
  }

 private:
  MatrixExpr expr_;
};

}  // namespace type_annotations
}  // namespace math
