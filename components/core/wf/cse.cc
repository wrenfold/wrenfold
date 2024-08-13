// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/cse.h"

#include <cstddef>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>

#include "wf/expression.h"
#include "wf/expression_cache.h"
#include "wf/expression_iteration.h"
#include "wf/expression_visitor.h"
#include "wf/utility/hashing.h"

namespace wf {

// Recursively count unique references to each expression in the tree.
class counter_visitor {
 public:
  counter_visitor() { counts_.reserve(10); }

  template <typename T>
  void operator()(const T& concrete) {
    for_each_child(concrete, [&](const auto& child) { record_count(child); });
  }

  template <typename ChildExprType>
  void record_count(const ChildExprType& child) {
    if constexpr (std::is_same_v<ChildExprType, scalar_expr>) {
      ++counts_[child];
    }
    if (const auto [_, was_inserted] = visited_.get<ChildExprType>().insert(child); was_inserted) {
      // Recurse into this expression if we haven't seen it yet.
      visit(child, *this);
    }
  }

  // Access counts.
  constexpr const auto& counts() const noexcept { return counts_; }

 private:
  template <typename Key>
  using visited_set_type = std::unordered_set<Key, hash_struct<Key>, is_identical_struct<Key>>;

  std::unordered_map<scalar_expr, std::size_t, hash_struct<scalar_expr>,
                     is_identical_struct<scalar_expr>>
      counts_;

  expression_map_tuple<visited_set_type> visited_;
};

cse_visitor::cse_visitor(const counter_visitor& counter, variable_function make_variable,
                         const std::size_t min_occurrences)
    : counter_(counter),
      make_variable_(std::move(make_variable)),
      min_occurrences_(min_occurrences) {
  if (!make_variable_) {
    make_variable_ = [](std::size_t index) { return scalar_expr{fmt::format("v{}", index)}; };
  }
  replacements_.reserve(10);
}

template <typename T, typename>
T cse_visitor::operator()(const T& expr) {
  return cache_.get_or_insert(expr, [this](const T& x) { return visit(x, *this); });
}

template <typename T, typename X>
X cse_visitor::operator()(const T& concrete, const X& expr) {
  if constexpr (!T::is_leaf_node) {
    // First visit children:
    X mapped = concrete.map_children(*this);

    // For scalars, check if we should insert a replacement
    if constexpr (std::is_same_v<X, scalar_expr>) {
      const auto& counts = counter_.counts();
      if (const auto it = counts.find(expr); it != counts.end() && it->second >= min_occurrences_) {
        // Introduce a new variable for this expression.
        const auto& [var, _] =
            replacements_.emplace_back(make_variable_(replacements_.size()), std::move(mapped));
        return var;
      }
    }

    return mapped;
  } else {
    // We don't replace leaf nodes.
    return expr;
  }
}

any_expression cse_visitor::operator()(const any_expression& expr) {
  return std::visit([this](const auto& x) -> any_expression { return this->operator()(x); }, expr);
}

template <typename T>
auto cse(const T& expr, std::function<scalar_expr(std::size_t)> make_variable,
         const std::size_t min_occurrences) {
  counter_visitor counter{};
  visit(expr, counter);

  cse_visitor cse{counter, std::move(make_variable), min_occurrences};
  T expr_out = visit(expr, cse);
  return std::make_tuple(std::move(expr_out), std::move(cse).take_replacements());
}

std::tuple<scalar_expr, std::vector<std::tuple<scalar_expr, scalar_expr>>> eliminate_subexpressions(
    const scalar_expr& expr, std::function<scalar_expr(std::size_t)> make_variable,
    const std::size_t min_occurrences) {
  return cse(expr, std::move(make_variable), min_occurrences);
}

std::tuple<matrix_expr, std::vector<std::tuple<scalar_expr, scalar_expr>>> eliminate_subexpressions(
    const matrix_expr& expr, std::function<scalar_expr(std::size_t)> make_variable,
    const std::size_t min_occurrences) {
  return cse(expr, std::move(make_variable), min_occurrences);
}

}  // namespace wf
