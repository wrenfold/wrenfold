#pragma once
#include "visitor_base.h"

namespace math {

// Abstract base for type-erased expressions. See `ExpressionImpl` for the implementation.
class ExpressionConcept {
 public:
  virtual ~ExpressionConcept() = default;

  // Construct w/ hash.
  explicit ExpressionConcept(std::size_t hash) : hash_(hash) {}

  // Test if two expressions are identical.
  virtual bool is_identical_to(const ExpressionConcept& other) const = 0;

  // Apply a visitor to this expression.
  virtual void receive_visitor(VisitorBase& visitor) const = 0;

  // Get the string name of the underlying expression.
  virtual std::string_view type_name() const = 0;

  // Is this expression a leaf-node type.
  virtual bool is_leaf() const = 0;

  // Check if the underlying derived type is one of `... Ts`.
  template <typename... Ts>
  bool is_type() const {
    static_assert((list_contains_type_v<Ts, ExpressionTypeList> && ...),
                  "Ts is not a valid expression type");
    // TODO: We can save a couple of percent if we save type_index on this object.
    return (type_matches_index(index_of_type_v<Ts, ExpressionTypeList>) || ...);
  }

  // Retrieve the hash of the expression.
  std::size_t get_hash() const { return hash_; }

  // Retrieve the type index.
  virtual std::size_t type_index() const = 0;

 protected:
  // True if the underlying type matches the provided index.
  virtual bool type_matches_index(std::size_t index) const = 0;

  // Hash of the derived type, which we compute on construction and cache here.
  std::size_t hash_{};
};

}  // namespace math
