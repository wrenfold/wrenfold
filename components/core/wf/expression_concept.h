#pragma once
#include "wf/visitor_base.h"

namespace math {

// Abstract base for type-erased expressions. See `expression_implementation` for the
// implementation.
class expression_concept {
 public:
  virtual ~expression_concept() = default;

  // Construct w/ hash.
  explicit expression_concept(std::size_t hash, std::size_t type_index)
      : hash_(hash), type_index_(type_index) {}

  // Test if two expressions are identical.
  virtual bool is_identical_to(const expression_concept& other) const = 0;

  // Get the string name of the underlying expression.
  virtual std::string_view type_name() const = 0;

  // Is this expression a leaf-node type.
  virtual bool is_leaf() const = 0;

  // Check if the underlying derived type is one of `... Ts`.
  template <typename... Ts>
  constexpr bool is_type() const noexcept {
    static_assert((type_list_contains_type_v<Ts, ExpressionTypeList> && ...),
                  "Ts is not a valid expression type");
    return ((index_of_type_v<Ts, ExpressionTypeList> == type_index_) || ...);
  }

  // Retrieve the hash of the expression.
  constexpr std::size_t get_hash() const noexcept { return hash_; }

  // Retrieve the type index.
  constexpr std::size_t type_index() const noexcept { return type_index_; }

 protected:
  // Hash of the derived type, which we compute on construction and cache here.
  std::size_t hash_{};

  // Index of the type.
  std::size_t type_index_{};
};

}  // namespace math
