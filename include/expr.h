#pragma once
#include <memory>
#include <string>

#include "binary_operation_utils.h"

namespace math {

// Forward declare.
class ExpressionBase;
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

/**
 * Wrapper around a pointer to an abstract expression. Defined so you can easily write chains of
 * operations without dealing with pointers at all.
 */
class Expr {
 public:
  // Constructors.
  explicit Expr(ExpressionBaseConstPtr&& impl) : impl_(std::move(impl)) {}
  explicit Expr(const ExpressionBaseConstPtr& impl) : impl_(impl) {}

  // Get the implementation pointer.
  const ExpressionBaseConstPtr& GetImpl() const { return impl_; }

  // Get a raw pointer.
  const ExpressionBase* GetRaw() const { return impl_.get(); }

  // Get a raw pointer, cast dynamically to a particular type.
  template <typename T>
  const T* GetRaw() const {
    return dynamic_cast<const T*>(impl_.get());
  }

  // Implicit cast to ExpressionBaseConstPtr
  operator const ExpressionBaseConstPtr&() const {  // NOLINT(google-explicit-constructor)
    return impl_;
  }

  // Convert to string.
  std::string ToString() const;

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int Reps = 1) const;

 private:
  ExpressionBaseConstPtr impl_;
};

// Operations defined on expressions:
inline Expr operator*(const Expr& a, const Expr& b) { return Expr{CreateMultiplication(a, b)}; }
inline Expr operator+(const Expr& a, const Expr& b) { return Expr{CreateAddition(a, b)}; }
inline Expr operator-(const Expr& a, const Expr& b) { return Expr{CreateSubtraction(a, b)}; }
inline Expr operator/(const Expr& a, const Expr& b) { return Expr{CreateDivision(a, b)}; }

// ostream support for libfmt
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToString();
  return stream;
}

}  // namespace math
