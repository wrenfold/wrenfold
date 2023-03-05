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

  virtual ~Expr() = default;

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const Expr& other) const { return impl_->IsIdenticalTo(*other.impl_); }

  // Test if the two expressions have the same underlying address.
  bool HasSameAddress(const Expr& other) const { return impl_.get() == other.impl_.get(); }

  // Useful for debugging sometimes: get the underlying address as void*.
  [[maybe_unused]] const void* GetAddress() const { return static_cast<const void*>(impl_.get()); }

  // Get the underlying type name as a string.
  std::string_view TypeName() const { return impl_->TypeName(); }

  // Whether this expression is a leaf node in the expression tree.
  bool IsLeaf() const { return impl_->IsLeaf(); }

  // Convert to string.
  std::string ToString() const;

  // Negation operator.
  Expr operator-() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int reps = 1) const;

  // Distribute terms in this expression.
  Expr Distribute() const;

  // Create a new expression by recursively substituting `replacement` for `target`.
  Expr Subs(const Expr& target, const Expr& replacement) const;

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
  static MatrixExpr Create(index_t rows, index_t cols, std::vector<Expr> args);

  // Get # of rows.
  index_t NumRows() const;

  // Get # of columns.
  index_t NumCols() const;

  // Size as size_t.
  std::size_t Size() const { return static_cast<std::size_t>(NumRows() * NumCols()); }

  // For vectors or row-vectors only. Access element `i`.
  const Expr& operator[](index_t i) const;

  // Access row `i` and column `j`.
  const Expr& operator()(index_t i, index_t j) const;

  // Get a block of rows [start, start + length).
  MatrixExpr GetBlock(index_t row, index_t col, index_t nrows, index_t ncols) const;

  // Transpose the matrix.
  [[nodiscard]] MatrixExpr Transpose() const;

  // Static cast to underlying matrix type.
  const Matrix& AsMatrix() const;
};

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

// Create a tuple of `Expr` from string arguments.
template <typename... Args>
auto Symbols(Args&&... args) {
  static_assert(std::disjunction_v<std::is_constructible<std::string_view, std::decay_t<Args>>...>,
                "Argument types must be coercible to string_view");
  return std::make_tuple(Expr{std::forward<Args>(args)}...);
}

}  // namespace math
