// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/utility_visitors.h"

#include "wf/expression_iteration.h"
#include "wf/expression_visitor.h"

namespace wf {

// Visitor that returns true for numerical values, or powers of numerical values.
struct is_numeric_visitor {
  template <typename T>
  bool operator()(const T& arg) const {
    if constexpr (type_list_contains_v<T, float_constant, integer_constant, rational_constant>) {
      return true;
    } else if constexpr (std::is_same_v<T, power>) {
      return is_numeric(arg.base()) && is_numeric(arg.exponent());
    } else {
      return false;
    }
  }
};

bool is_numeric(const scalar_expr& expr) { return visit(expr, is_numeric_visitor{}); }

// Visitor that identifies negative numeric constants, or products of numeric constants that will be
// negative.
struct is_negative_number_visitor {
  constexpr bool operator()(const integer_constant& i) const noexcept { return i.is_negative(); }
  constexpr bool operator()(const float_constant& f) const noexcept { return f.is_negative(); }
  constexpr bool operator()(const rational_constant& r) const noexcept { return r.is_negative(); }

  template <typename T>
  constexpr bool operator()(const T&) const noexcept {
    return false;
  }

  // Multiplications can be negative-like, if the product of all the constant terms is negative.
  bool operator()(const multiplication& m) const {
    const std::size_t count = std::count_if(m.begin(), m.end(), &is_negative_number);
    // odd = negative, even = positive
    return static_cast<bool>(count & 1);
  }
};

bool is_negative_number(const scalar_expr& expr) {
  return visit(expr, is_negative_number_visitor{});
}

template <typename T>
void get_variables_visitor::operator()(const T& arg) {
  if constexpr (std::is_same_v<T, scalar_expr>) {
    if (!visited_.count(arg)) {
      visited_.insert(arg);
      visit(arg, *this);
    }
  } else if constexpr (inherits_expression_base_v<T>) {
    visit(arg, *this);
  } else if constexpr (std::is_same_v<T, variable>) {
    variables_.push_back(arg);
  } else {
    for_each_child(arg, *this);
  }
}

std::vector<variable> get_variables(const scalar_expr& expr) {
  get_variables_visitor visitor;
  visit(expr, visitor);
  return std::move(visitor).take_output();
}

template <typename T>
template <typename U>
bool is_function_of_visitor<T>::operator()(const U& x) const {
  if constexpr (inherits_expression_base_v<U>) {
    return visit(x, *this);
  } else if constexpr (std::is_same_v<U, any_expression>) {
    return std::visit(*this, x);
  } else if constexpr (std::is_same_v<U, T>) {
    return are_identical(target_, x);
  } else if constexpr (!U::is_leaf_node) {
    return any_of(x.children(), *this);
  } else {
    return false;
  }
}

bool is_function_of(const scalar_expr& func, const scalar_expr& target) {
  return visit(target, [&func](const auto& target_concrete) -> bool {
    return is_function_of_visitor(target_concrete)(func);
  });
}

}  // namespace wf
