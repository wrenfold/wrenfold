#pragma once
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
      : ExpressionConcept(ComputeHash(impl)), implementation_(std::move(impl)) {}

  // Cast to the concrete expression type. type.
  const ExpressionType& GetImplementation() const { return implementation_; }

  // Check if we can cast to this type.
  bool IsIdenticalTo(const ExpressionConcept& other) const override final {
    return other.IsType<ExpressionType>() &&
           GetImplementation().IsIdenticalTo(
               static_cast<const ExpressionImpl<ExpressionType>&>(other).GetImplementation());
  }

  // Cast to derived type and apply the visitor.
  void Receive(VisitorBase& visitor) const override final {
    static_cast<VisitorDeclare<ExpressionType>&>(visitor).ApplyVirtual(GetImplementation());
  }

  // Get the derived type string name (a static constexpr member).
  std::string_view TypeName() const override final { return ExpressionType::NameStr; }

  // Whether the derived type is a leaf (it contains no references to child expressions).
  static constexpr bool IsLeafStatic() { return ExpressionType::IsLeafNode; }

  // Virtual version of `IsLeafStatic`.
  bool IsLeaf() const override final { return IsLeafStatic(); }

  // Implementation of `TypeIndex`.
  std::size_t TypeIndex() const override final {
    return IndexOfType<ExpressionType, ExpressionTypeList>::Value;
  }

 protected:
  bool TypeMatchesIndex(const std::size_t index) const override final {
    static_assert(ContainsType<ExpressionType, ExpressionTypeList>,
                  "ExpressionType is not a valid expression type");
    return IndexOfType<ExpressionType, ExpressionTypeList>::Value == index;
  }

  // Compute the hash of the underlying expression, and hash it again w/ the index of the type.
  static std::size_t ComputeHash(const ExpressionType& impl) {
    return HashCombine(IndexOfType<ExpressionType, ExpressionTypeList>::Value,
                       Hash<ExpressionType>{}(impl));
  }

  ExpressionType implementation_;
};

// Create an `Expr` with underlying type `T` and constructor args `Args`.
template <typename T, typename... Args>
Expr MakeExpr(Args&&... args) {
  return Expr{std::make_shared<const ExpressionImpl<T>>(T{std::forward<Args>(args)...})};
}

// Cast expression to const pointer of the specified type.
// Returned pointer is valid in scope only as long as the argument `x` survives.
template <typename T>
const T* CastPtr(const Expr& x) {
  if (x.impl_->IsType<T>()) {
    const T& concrete = static_cast<const ExpressionImpl<T>*>(x.impl_.get())->GetImplementation();
    return &concrete;
  } else {
    return nullptr;
  }
}

// Cast expression `x` to type `T` if possible, and check if it is identical
// to `typed_y`.
template <typename T>
bool IsIdenticalToConcrete(const Expr& x, const T& typed_y) {
  const T* as_typed = CastPtr<T>(x);
  if (!as_typed) {
    return false;
  }
  return as_typed->IsIdenticalTo(typed_y);
}

// Traverse all sub-expressions of the input expression. The provided `operation` will be called
// once on each child.
template <typename ExpressionType, typename Operation>
void IterateChildren(const ExpressionType& expr, Operation&& operation) {
  constexpr bool is_leaf = ExpressionType::IsLeafNode;
  if constexpr (!is_leaf) {
    expr.Iterate(std::forward<Operation>(operation));
  }
}

// Create a copy of an expression by running a unary map on its child expressions.
// The derived type should copy itself w/ new children, leaving any other properties identical.
template <typename ExpressionType, typename Operation>
std::enable_if_t<!ExpressionType::IsLeafNode, Expr> MapChildren(const ExpressionType& expr,
                                                                Operation&& operation) {
  return expr.Map(std::forward<Operation>(operation));
}

}  // namespace math