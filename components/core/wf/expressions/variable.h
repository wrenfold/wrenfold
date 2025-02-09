// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/expression.h"
#include "wf/utility/ordering.h"

namespace wf {

// A type that is substituted into user-provided expressions during code-generation.
// Typically, the user does not create these directly.
class function_argument_variable {
 public:
  static constexpr std::string_view name_str = "FunctionArgumentVariable";
  static constexpr bool is_leaf_node = true;

  constexpr function_argument_variable(const std::size_t arg_index, const std::size_t element_index,
                                       const numeric_primitive_type primitive_type) noexcept
      : arg_index_(arg_index), element_index_(element_index), primitive_type_(primitive_type) {}

  // constexpr bool operator<(const function_argument_variable& other) const noexcept {
  //   return std::make_tuple(arg_index_, element_index_, primitive_type_) <
  //          std::make_tuple(other.arg_index_, other.element_index_, other.primitive_type_);
  // }
  // constexpr bool operator==(const function_argument_variable& other) const noexcept {
  //   return arg_index_ == other.arg_index_ && element_index_ == other.element_index_ &&
  //          primitive_type_ == other.primitive_type_;
  // }

  // Which function argument this refers to.
  constexpr std::size_t arg_index() const noexcept { return arg_index_; }

  // Index into the function argument.
  // For a matrix arg, this is a flat index into the matrix.
  constexpr std::size_t element_index() const noexcept { return element_index_; }

  // The numeric type of this variable.
  constexpr numeric_primitive_type primitive_type() const noexcept { return primitive_type_; }

 private:
  std::size_t arg_index_;
  std::size_t element_index_;
  // TODO: `numeric_primitive_type` is a bit too liberal, since it also incorporates boolean.
  // But boolean can only be used in boolean_expr, not scalar_expr.
  numeric_primitive_type primitive_type_;
};

// A variable designated by a unique integer index that is not reused.
class unique_variable {
 public:
  static constexpr std::string_view name_str = "UniqueVariable";
  static constexpr bool is_leaf_node = true;

  // Create a new variable w/ the next index.
  explicit unique_variable(const number_set set)
      : index_{next_unique_variable_index()}, set_(set) {}

  // constexpr bool operator<(const unique_variable& other) const noexcept {
  //   return std::make_tuple(index_, set_) < std::make_tuple(other.index_, other.set_);
  // }
  // constexpr bool operator==(const unique_variable& other) const noexcept {
  //   return std::make_tuple(index_, set_) == std::make_tuple(other.index_, other.set_);
  // }

  // Retrieve the underlying index.
  constexpr std::size_t index() const noexcept { return index_; }

  // Numeric set the unique variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

 private:
  static std::size_t next_unique_variable_index();

  std::size_t index_;
  number_set set_;
};

// A variable w/ a user-provided name.
class variable {
 public:
  static constexpr std::string_view name_str = "Variable";
  static constexpr bool is_leaf_node = true;

  explicit variable(std::string name, const number_set set) noexcept
      : name_{std::move(name)}, set_(set) {}

  // constexpr bool operator==(const named_variable& other) const noexcept {
  //   return std::make_tuple(name_, set_) == std::make_tuple(other.name_, other.set_);
  // }

  // String name of the variable.
  constexpr const std::string& name() const noexcept { return name_; }

  // Numeric set the variable belongs to.
  constexpr number_set set() const noexcept { return set_; }

 private:
  std::string name_;
  number_set set_;
};

// A named variable used in an expression.
// This can be one of three underlying types: named_variable, function_argument_variable,
// unique_variable.
// class variable {
//  public:
//   static constexpr std::string_view name_str = "Variable";
//   static constexpr bool is_leaf_node = true;

//   // There are different kinds of variables, but the different kinds don't matter to most
//   visitors.
//   // We don't expose that information at the expression level, and instead store it inside
//   Variable. using identifier_type = std::variant<named_variable, function_argument_variable,
//   unique_variable>;

//   // Construct variable from user-provided name.
//   variable(std::string name, const number_set set)
//       : identifier_{std::in_place_type_t<named_variable>(), std::move(name), set} {}

//   explicit variable(identifier_type identifier) noexcept : identifier_(std::move(identifier)) {}

//   // Access the variant of different variable representations.
//   constexpr const auto& identifier() const noexcept { return identifier_; }

//   // The numeric set this variable belongs to.
//   constexpr number_set set() const noexcept {
//     struct visitor {
//       constexpr number_set operator()(const named_variable& x) const { return x.set(); }
//       constexpr number_set operator()(const unique_variable& x) const { return x.set(); }
//       constexpr number_set operator()(const function_argument_variable&) const {
//         return number_set::real;
//       }
//     };
//     return std::visit(visitor{}, identifier_);
//   }

//   // True if this variable is a `unique_variable`.
//   constexpr bool is_unique_variable() const noexcept {
//     return std::holds_alternative<unique_variable>(identifier_);
//   }

//   // Convert the identifier to a string.
//   std::string to_string() const;

//   // Create a function argument expression.
//   static scalar_expr create_function_argument(std::size_t arg_index, std::size_t element_index,
//                                               numeric_primitive_type type);

//  private:
//   identifier_type identifier_;
// };

scalar_expr create_function_argument(std::size_t arg_index, std::size_t element_index,
                                     numeric_primitive_type type);

template <>
struct hash_struct<variable> {
  std::size_t operator()(const variable& v) const noexcept {
    return hash_combine(hash_string_fnv(v.name()), static_cast<std::size_t>(v.set()));
  }
};

template <>
struct is_identical_struct<variable> {
  bool operator()(const variable& a, const variable& b) const noexcept {
    return a.name() == b.name() && a.set() == b.set();
  }
};

template <>
struct order_struct<variable> {
  constexpr relative_order operator()(const variable& a, const variable& b) const noexcept {
    return order_by(a.name(), b.name()).and_then_by(a.set(), b.set());
  }
};

template <>
struct hash_struct<unique_variable> {
  constexpr std::size_t operator()(const unique_variable& v) const noexcept {
    return hash_combine(v.index(), static_cast<std::size_t>(v.set()));
  }
};

template <>
struct is_identical_struct<unique_variable> {
  constexpr bool operator()(const unique_variable& a, const unique_variable& b) const noexcept {
    return a.index() == b.index() && a.set() == b.set();
  }
};

template <>
struct order_struct<unique_variable> {
  constexpr relative_order operator()(const unique_variable& a,
                                      const unique_variable& b) const noexcept {
    return order_by(a.index(), b.index()).and_then_by(a.set(), b.set());
  }
};

template <>
struct hash_struct<function_argument_variable> {
  constexpr std::size_t operator()(const function_argument_variable& v) const noexcept {
    return hash_combine(hash_combine(v.arg_index(), v.element_index()),
                        static_cast<std::size_t>(v.primitive_type()));
  }
};

template <>
struct is_identical_struct<function_argument_variable> {
  constexpr bool operator()(const function_argument_variable& a,
                            const function_argument_variable& b) const noexcept {
    return a.arg_index() == b.arg_index() && a.element_index() == b.element_index() &&
           a.primitive_type() == b.primitive_type();
  }
};

template <>
struct order_struct<function_argument_variable> {
  constexpr relative_order operator()(const function_argument_variable& a,
                                      const function_argument_variable& b) const noexcept {
    return order_by(a.arg_index(), b.arg_index())
        .and_then_by(a.element_index(), b.element_index())
        .and_then_by(a.primitive_type(), b.primitive_type());
  }
};

// template <>
// struct hash_struct<variable> {
//   std::size_t operator()(const variable& v) const noexcept {
//     return hash_variant<variable::identifier_type>{}(v.identifier());
//   }
// };

// template <>
// struct is_identical_struct<variable> {
//   bool operator()(const variable& a, const variable& b) const {
//     // Comparing identifiers will use the std::variant::operator==.
//     return a.identifier() == b.identifier();
//   }
// };

// template <>
// struct order_struct<variable> {
//   relative_order operator()(const variable& a, const variable& b) const {
//     // Invoking std::variant::operator<
//     if (a.identifier() < b.identifier()) {
//       return relative_order::less_than;
//     } else if (are_identical(a, b)) {
//       return relative_order::equal;
//     }
//     return relative_order::greater_than;
//   }
// };

}  // namespace wf
