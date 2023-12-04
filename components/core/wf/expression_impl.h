#pragma once
#include "wf/error_types.h"
#include "wf/expression.h"
#include "wf/hashing.h"
#include "wf/visitor_base.h"

namespace math {

// Child of expression_concept.
// Stores the concrete expression type and implements virtual methods.
template <typename ExpressionType>
class expression_implementation final : public expression_concept {
 public:
  ~expression_implementation() override = default;

  // Construct w/ concrete inner type.
  explicit expression_implementation(ExpressionType&& impl)
      : expression_concept(compute_hash(impl), index_of_type_v<ExpressionType, ExpressionTypeList>),
        implementation_(std::move(impl)) {}

  // Cast to the concrete expression type. type.
  constexpr const ExpressionType& get_implementation() const noexcept { return implementation_; }

  // Check if we can cast to this type.
  bool is_identical_to(const expression_concept& other) const override final {
    return other.is_type<ExpressionType>() &&
           get_implementation().is_identical_to(
               static_cast<const expression_implementation<ExpressionType>&>(other)
                   .get_implementation());
  }

  // Get the derived type string name (a static constexpr member).
  std::string_view type_name() const override final { return ExpressionType::name_str; }

  // Whether the derived type is a leaf (it contains no references to child expressions).
  static constexpr bool IsLeafStatic() { return ExpressionType::is_leaf_node; }

  // Virtual version of `IsLeafStatic`.
  bool is_leaf() const override final { return IsLeafStatic(); }

 protected:
  // Compute the hash of the underlying expression, and hash it again w/ the index of the type.
  static std::size_t compute_hash(const ExpressionType& impl) {
    return hash_combine(index_of_type<ExpressionType, ExpressionTypeList>::value,
                        hash_struct<ExpressionType>{}(impl));
  }

  ExpressionType implementation_;
};

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr make_expr(Args&&... args) {
  return Expr{std::make_shared<const expression_implementation<T>>(T{std::forward<Args>(args)...})};
}

// Cast expression to const pointer of the specified type.
// Returned pointer is valid in scope only as long as the argument `x` survives.
template <typename T>
const T* cast_ptr(const Expr& x) {
  if (x.impl_->is_type<T>()) {
    const T& concrete =
        static_cast<const expression_implementation<T>*>(x.impl_.get())->get_implementation();
    return &concrete;
  } else {
    return nullptr;
  }
}

// Cast expression to const reference of the specified type. TypeError is thrown if the cast is
// invalid.
template <typename T>
const T& cast_checked(const Expr& x) {
  if (x.impl_->is_type<T>()) {
    return static_cast<const expression_implementation<T>*>(x.impl_.get())->get_implementation();
  } else {
    throw type_error("Cannot cast expression of type `{}` to `{}`", x.type_name(), T::name_str);
  }
}

// Cast expression with no checking. UB will occur if the wrong type is accessed.
template <typename T>
const T& cast_unchecked(const Expr& x) {
  return static_cast<const expression_implementation<T>*>(x.impl_.get())->get_implementation();
}

}  // namespace math
