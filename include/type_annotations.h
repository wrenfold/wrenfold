// Copyright 2023 Gareth Cross
#pragma once
#include "assertions.h"
#include "matrix_functions.h"

namespace math {

// Store information about an argument to a function.
// User specifies this as an argument to `BuildFunctionDescription`.
struct Arg {
  Arg() = default;
  explicit Arg(const std::string_view name, bool optional = false)
      : name_(name), optional_(optional) {}

  const std::string& GetName() const { return name_; }

  bool IsOptional() const { return optional_; }

 private:
  std::string name_;
  bool optional_{false};
};

namespace type_annotations {

template <index_t Rows, index_t Cols>
struct StaticMatrix {
  // Allow implicit construction from MatrixExpr.
  StaticMatrix(MatrixExpr expr) : expr_(std::move(expr)) {  // NOLINT(google-explicit-constructor)
    ASSERT_EQUAL(Rows, expr_.NumRows());
    ASSERT_EQUAL(Cols, expr_.NumCols());
  }

  template <typename... Args>
  static std::enable_if_t<std::conjunction_v<std::is_constructible<Expr, Args>...>, StaticMatrix>
  Create(Args&&... args) {
    static_assert(sizeof...(args) == Rows * Cols);
    return StaticMatrix(CreateMatrix(Rows, Cols, std::move(args)...));
  }

  const Expr& operator()(index_t row, index_t col) const { return expr_(row, col); }

  // Access vector element.
  const Expr& operator[](index_t element) const { return expr_[element]; }

  // Assign from MatrixExpr
  StaticMatrix& operator=(const MatrixExpr& other) {
    ASSERT_EQUAL(Rows, other.NumRows());
    ASSERT_EQUAL(Cols, other.NumCols());
    expr_ = other;
    return *this;
  }

  // Assign from Expr
  StaticMatrix& operator=(const Expr& other) { return operator=(static_cast<MatrixExpr>(other)); }

  // Implicit cast to MatrixExpr.
  operator const MatrixExpr&() const { return expr_; }  // NOLINT

  // Transpose:
  StaticMatrix<Cols, Rows> Transpose() const { return StaticMatrix<Cols, Rows>{expr_.Transpose()}; }

 private:
  MatrixExpr expr_;
};

}  // namespace type_annotations
}  // namespace math
