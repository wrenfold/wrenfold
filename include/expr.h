#pragma once
#include <memory>
#include <string>

#include "formatting_fwd.h"
#include "operation_utils.h"

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

  // Construct variable:
  explicit Expr(const StringType& name);

#ifdef USE_WIDE_STR
  // For convenience, support construction from plain string as well.
  explicit Expr(const std::string& name);
#endif

  // Construct constant. Implicit so we can use numbers in math operations.
  Expr(double x);

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

  // Test if the two expressions are identical.
  bool IsIdenticalTo(const Expr& other) const;

  // Convert to string.
  StringType ToString() const { return ToPlainString(impl_); }

  // Convert to a string, always returning a plain std::string.
  std::string ToNarrowString() const { return ToPlainNarrowString(impl_); }

  // Negation operator.
  Expr operator-() const { return Expr{CreateNegation(impl_)}; }

  // Differentiate wrt a single variable. Reps defines how many derivatives to take.
  Expr Diff(const Expr& var, int Reps = 1) const;

 private:
  ExpressionBaseConstPtr impl_;
};

template <typename T>
struct IsConvertibleToExpr {
  static constexpr bool value = std::is_same<T, Expr>::value || std::is_arithmetic<T>::value;
};

template <typename T>
using EnableConvertibleToExpr = std::enable_if<IsConvertibleToExpr<T>::value, Expr>;

// Operations defined on expressions:
inline Expr operator*(const Expr& a, const Expr& b) { return Expr{CreateMultiplication(a, b)}; }
inline Expr operator+(const Expr& a, const Expr& b) { return Expr{CreateAddition(a, b)}; }
inline Expr operator-(const Expr& a, const Expr& b) { return Expr{CreateSubtraction(a, b)}; }
inline Expr operator/(const Expr& a, const Expr& b) { return Expr{CreateDivision(a, b)}; }
inline Expr operator^(const Expr& a, const Expr& b) { return Expr{CreatePower(a, b)}; }

// ostream support
inline std::ostream& operator<<(std::ostream& stream, const Expr& x) {
  stream << x.ToNarrowString();
  return stream;
}

#ifdef USE_WIDE_STR
inline std::wostream& operator<<(std::wostream& stream, const Expr& x) {
  stream << x.ToString();
  return stream;
}
#else
inline std::wostream& operator<<(std::wostream& stream, const Expr& x) {
  stream << WideFromNarrow(x.ToString());
  return stream;
}
#endif

}  // namespace math
