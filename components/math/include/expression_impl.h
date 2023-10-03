#pragma once
#include "error_types.h"
#include "expression.h"
#include "hashing.h"
#include "visitor_base.h"

namespace math {

// Child of ExpressionConcept.
// Stores the concrete expression type and implements virtual methods.
template <typename ExpressionType>
class ExpressionImpl final : public ExpressionConcept {
 public:
  ~ExpressionImpl() override = default;

  // Construct w/ concrete inner type.
  explicit ExpressionImpl(ExpressionType&& impl)
      : ExpressionConcept(compute_hash(impl)), implementation_(std::move(impl)) {}

  // Cast to the concrete expression type. type.
  const ExpressionType& get_implementation() const { return implementation_; }

  // Check if we can cast to this type.
  bool is_identical_to(const ExpressionConcept& other) const override final {
    return other.is_type<ExpressionType>() &&
           get_implementation().is_identical_to(
               static_cast<const ExpressionImpl<ExpressionType>&>(other).get_implementation());
  }

  // Cast to derived type and apply the visitor.
  void receive_visitor(VisitorBase& visitor) const override final {
    static_cast<VisitorDeclare<ExpressionType>&>(visitor).apply_visitor_virtual(
        get_implementation());
  }

  // Get the derived type string name (a static constexpr member).
  std::string_view type_name() const override final { return ExpressionType::NameStr; }

  // Whether the derived type is a leaf (it contains no references to child expressions).
  static constexpr bool IsLeafStatic() { return ExpressionType::IsLeafNode; }

  // Virtual version of `IsLeafStatic`.
  bool is_leaf() const override final { return IsLeafStatic(); }

  // Implementation of `TypeIndex`.
  std::size_t type_index() const override final {
    return index_of_type<ExpressionType, ExpressionTypeList>::value;
  }

 protected:
  bool type_matches_index(const std::size_t index) const override final {
    static_assert(list_contains_type_v<ExpressionType, ExpressionTypeList>,
                  "ExpressionType is not a valid expression type");
    return index_of_type<ExpressionType, ExpressionTypeList>::value == index;
  }

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
  return Expr{std::make_shared<const ExpressionImpl<T>>(T{std::forward<Args>(args)...})};
}

// Cast expression to const pointer of the specified type.
// Returned pointer is valid in scope only as long as the argument `x` survives.
template <typename T>
const T* cast_ptr(const Expr& x) {
  if (x.impl_->is_type<T>()) {
    const T& concrete = static_cast<const ExpressionImpl<T>*>(x.impl_.get())->get_implementation();
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
    return static_cast<const ExpressionImpl<T>*>(x.impl_.get())->get_implementation();
  } else {
    throw TypeError("Cannot cast expression of type `{}` to `{}`", x.type_name(), T::NameStr);
  }
}

// Cast expression with no checking. UB will occur if the wrong type is accessed.
template <typename T>
const T& cast_unchecked(const Expr& x) {
  return static_cast<const ExpressionImpl<T>*>(x.impl_.get())->get_implementation();
}

}  // namespace math
