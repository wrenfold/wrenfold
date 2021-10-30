#pragma once
#include <memory>
#include <string>

#include <fmt/ostream.h>

namespace math {

class ExpressionBase;
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

class NumericConstants {
public:
  static const ExpressionBaseConstPtr Zero;
  static const ExpressionBaseConstPtr One;
};

class Expr {
public:
  Expr(ExpressionBaseConstPtr &&impl) : impl_(std::move(impl)) {}

  // Get the implementation pointer.
  const ExpressionBaseConstPtr &GetImpl() const { return impl_; }

  // Get a raw pointer.
  const ExpressionBase *GetRaw() const { return impl_.get(); }

  // Get a raw pointer, casted dynamically to a particular type.
  template <typename T> const T *GetRaw() const {
    return dynamic_cast<const T *>(impl_.get());
  }

  // Implicit cast to ExpressionBaseConstPtr
  operator const ExpressionBaseConstPtr &() const { return impl_; }

  // Convert to string.
  std::string ToString() const;

  // Differentiate wrt a single variable.
  Expr Diff(const Expr &var, int Reps = 1) const;

private:
  ExpressionBaseConstPtr impl_;
};

class Variable;

class ExpressionBase {
public:
  virtual ~ExpressionBase() {}

  // Differentiate.
  virtual ExpressionBaseConstPtr Diff(const Variable &var) const = 0;

  // Convert to string.
  virtual std::string ToString() const = 0;

  // Cast to particular type.
  template <typename T> const T *As() const {
    return dynamic_cast<const T *>(this);
  }

  // Test for equality.
  bool Equals(const ExpressionBase &other) const {
    if (this == &other) {
      // Do simple pointer equality check first.
      return true;
    }
    return EqualsImpl(other);
  }

  // Variant that accepts pointer.
  bool Equals(const ExpressionBaseConstPtr &other_ptr) const {
    return Equals(*other_ptr);
  }

protected:
  virtual bool EqualsImpl(const ExpressionBase &other) const = 0;
};

template <typename T, typename... Args>
ExpressionBaseConstPtr MakeExprBase(Args &&... args) {
  return std::make_shared<const T>(std::forward<Args>(args)...);
}

template <typename Derived> class ExpressionImpl : public ExpressionBase {
public:
  virtual ~ExpressionImpl() {}

protected:
  bool EqualsImpl(const ExpressionBase &other) const override {
    const Derived *const typed_other = other.As<Derived>();
    return typed_other &&
           static_cast<const Derived *>(this)->EqualsImplTyped(*typed_other);
  }

private:
};

class Constant : public ExpressionImpl<Constant> {
public:
  using ConstantType = double;

  explicit Constant(ConstantType val) : val_(val) {}

  // Differentiating a constant produces zero.
  ExpressionBaseConstPtr Diff(const Variable &) const override {
    return NumericConstants::Zero;
  }

  std::string ToString() const override { return fmt::format("{}", val_); }

  bool EqualsImplTyped(const Constant &other) const {
    return val_ == other.val_;
  }

private:
  ConstantType val_;
};

// TODO(gareth): Support different types.
template <typename T> Expr MakeNum(T &&x) {
  return Expr{MakeExprBase<Constant>(std::forward<T>(x))};
}

// Variable
class Variable : public ExpressionImpl<Variable> {
public:
  explicit Variable(const std::string &name) : name_(name) {}

  const std::string &GetName() const { return name_; }

  ExpressionBaseConstPtr Diff(const Variable &var) const override {
    if (EqualsImplTyped(var)) {
      return NumericConstants::One;
    }
    return NumericConstants::Zero;
  }

  std::string ToString() const override { return name_; }

  bool EqualsImplTyped(const Variable &other) const {
    return name_ == other.name_;
  }

private:
  std::string name_;
};

inline Expr MakeVar(const std::string &name) {
  return Expr(std::make_shared<Variable>(name));
}

template <typename Derived> class BinaryOp : public ExpressionImpl<Derived> {
public:
  BinaryOp(const ExpressionBaseConstPtr &a, const ExpressionBaseConstPtr &b)
      : a_(a), b_(b) {}

  BinaryOp(ExpressionBaseConstPtr &&a, ExpressionBaseConstPtr &&b)
      : a_(std::move(a)), b_(std::move(b)) {}

  // This will only be called for things with the same derived type, so we don't
  // need to check the specific operator here.
  bool EqualsImplTyped(const BinaryOp<Derived> &other) const {
    if (a_->Equals(other.a_) && b_->Equals(other.b_)) {
      return true;
    }
    if (Derived::IsCommutative) {
      if (a_->Equals(other.b_) && b_->Equals(other.a_)) {
        return true;
      }
    }
    return false;
  }

protected:
  std::string FormatString(const std::string &op) const {
    // TODO: figure out when the braces are required, and when not:
    return fmt::format("({} {} {})", a_->ToString(), op, b_->ToString());
  }

protected:
  ExpressionBaseConstPtr a_;
  ExpressionBaseConstPtr b_;
};

ExpressionBaseConstPtr CreateMultiplication(const ExpressionBaseConstPtr &a,
                                            const ExpressionBaseConstPtr &b);

ExpressionBaseConstPtr CreateAddition(const ExpressionBaseConstPtr &a,
                                      const ExpressionBaseConstPtr &b);

class Addition : public BinaryOp<Addition> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;

  ExpressionBaseConstPtr Diff(const Variable &var) const override {
    return CreateAddition(a_->Diff(var), b_->Diff(var));
  }

  std::string ToString() const { return FormatString("+"); }

private:
};

class Multiplication : public BinaryOp<Multiplication> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = true;

  ExpressionBaseConstPtr Diff(const Variable &var) const override {
    return CreateAddition(CreateMultiplication(a_, b_->Diff(var)),
                          CreateMultiplication(a_->Diff(var), b_));
  }

  std::string ToString() const { return FormatString("*"); }

private:
};

#if 0
class Division : public BinaryOp<Division> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable& var) const override {
    return CreateMultiplication(  );
  }

  std::string ToString() const { return FormatString("÷"); }

private:
};
#endif

template <typename T, typename... Args> auto MakeExpr(Args &&... args) {
  return Expr(MakeExprBase<T>(std::forward<Args>(args)...));
}

template <typename Derived>
class UnaryOp : public ExpressionImpl<UnaryOp<Derived>> {
public:
  explicit UnaryOp(const ExpressionBaseConstPtr &x) : x_(x) {}
  explicit UnaryOp(ExpressionBaseConstPtr &&x) : x_(std::move(x)) {}

protected:
  ExpressionBaseConstPtr x_;
};

#if 0
class NaturalLog : public UnaryOp<NaturalLog> {
public:
  using UnaryOp::UnaryOp;

  ExpressionBaseConstPtr Diff(const Variable &var) const override {}

  std::string ToString() const { return fmt::format("ln({})", x_->ToString()); }

private:
};

class Power : public BinaryOp<Power> {
public:
  using BinaryOp::BinaryOp;
  static constexpr bool IsCommutative = false;

  ExpressionBaseConstPtr Diff(const Variable &var) const override { return {}; }

  std::string ToString() const { return FormatString("^"); }
};
#endif 

bool IsZero(const ExpressionBaseConstPtr &expr);
bool IsOne(const ExpressionBaseConstPtr &expr);

inline Expr operator*(const Expr &a, const Expr &b) {
  return Expr{CreateMultiplication(a, b)};
}

inline Expr operator+(const Expr &a, const Expr &b) {
  return Expr{CreateAddition(a, b)};
}

inline std::ostream &operator<<(std::ostream &stream, const Expr &x) {
  stream << x.ToString();
  return stream;
}

} // namespace math
