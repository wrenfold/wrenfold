#pragma once
#include <memory>
#include <string>

#include "expression_concept.h"

namespace math {

class MatrixExpr;

/**
 * Wrapper around a pointer to an abstract expression. Defined so you can easily write chains of
 * operations without dealing with pointers at all.
 */
class Expr {
 public:
  // Constructors.
  explicit Expr(ExpressionConceptConstPtr&& impl) : impl_(std::move(impl)) {}
  explicit Expr(const ExpressionConceptConstPtr& impl) : impl_(impl) {}

  // Construct variable:
  explicit Expr(std::string_view name);

  // Implicit construction from integers and floats.
  // enable_if argument is a trick we use until c++20 and constraints.
  template <typename T>
  Expr(T v, std::enable_if_t<std::is_integral_v<T> || std::is_floating_point_v<T>, void*> = nullptr)
      : Expr(ConstructImplicit(v)) {}

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const Expr& other) const { return impl_->IsIdenticalTo(*other.impl_); }

  // Get the underlying type name as a string.
  std::string_view TypeName() const { return impl_->TypeName(); }

  // Convert to string.
  std::string ToString() const;

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int reps = 1) const;

  // Distribute terms in this expression.
  Expr Distribute() const;

  // Receive a visitor.
  void Receive(VisitorBase& visitor) const { impl_->Receive(visitor); }

 protected:
  [[nodiscard]] const ExpressionConceptConstPtr& Impl() const { return impl_; }

 private:
  // Construct constant from float.
  static Expr FromFloat(double x);

  // Construct from integer.
  static Expr FromInt(std::int64_t x);

  // TODO: Use checked casts here + safe numeric type.
  template <typename T>
  static Expr ConstructImplicit(T v) {
    static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
    if constexpr (std::is_integral_v<T>) {
      return FromInt(static_cast<std::int64_t>(v));
    } else if constexpr (std::is_floating_point_v<T>) {
      return FromFloat(static_cast<double>(v));
    }
  }

  ExpressionConceptConstPtr impl_;
};

static_assert(std::is_move_assignable_v<Expr> && std::is_move_constructible_v<Expr>,
              "Should be movable");

// Child of `Expr` that exposes certain operations only valid on matrices.
class MatrixExpr : public Expr {
 public:
  // Construct from expression. The underlying type is checked and an exception will be thrown
  // if the argument is not a matrix.
  explicit MatrixExpr(Expr&& arg);
  explicit MatrixExpr(const Expr& arg);

  // Static constructor: Create a dense matrix of expressions.
  static MatrixExpr CreateMatrix(std::size_t rows, std::size_t cols, std::vector<Expr> args);

  // Get # of rows.
  std::size_t NumRows() const;

  // Get # of columns.
  std::size_t NumCols() const;

  // For vectors or row-vectors only. Access element `i`.
  const Expr& operator[](std::size_t i) const;

  // Access row `i` and column `j`.
  const Expr& operator()(std::size_t i, std::size_t j) const;

  // Transpose the matrix.
  [[nodiscard]] MatrixExpr Transpose() const;

 protected:
  // Static cast to underlying matrix type.
  const Matrix& AsMatrix() const;
};

// Create a column vector from the arguments.
template <typename... Ts>
static MatrixExpr Vector(Ts&&... args) {
  return MatrixExpr::CreateMatrix(sizeof...(Ts), 1, {Expr{std::forward<Ts>(args)}...});
}

// Create a row vector from the arguments.
template <typename... Ts>
static MatrixExpr RowVector(Ts&&... args) {
  return MatrixExpr::CreateMatrix(1, sizeof...(Ts), {Expr{std::forward<Ts>(args)}...});
}

// Create a matrix from the arguments (args specified in row-major order).
template <typename... Ts>
static MatrixExpr CreateMatrix(std::size_t rows, std::size_t cols, Ts&&... args) {
  return MatrixExpr::CreateMatrix(rows, cols, {Expr{std::forward<Ts>(args)}...});
}

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToString();
  return stream;
}

// Math operators.
Expr operator+(const Expr& a, const Expr& b);
Expr operator-(const Expr& a, const Expr& b);
Expr operator*(const Expr& a, const Expr& b);
Expr operator/(const Expr& a, const Expr& b);

// Custom literal suffix support.
namespace custom_literals {
inline Expr operator"" _s(unsigned long long int arg) { return Expr{arg}; }
inline Expr operator"" _s(long double arg) { return Expr{arg}; }
}  // namespace custom_literals

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{std::make_shared<const T>(std::forward<Args>(args)...)};
}

}  // namespace math
