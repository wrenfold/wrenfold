// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <variant>

#include "wf/code_generation/types.h"
#include "wf/expressions/custom_type_expressions.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/expressions/special_constants.h"
#include "wf/expressions/variable.h"
#include "wf/external_function.h"
#include "wf/hashing.h"

// Define types for a very simple "intermediate representation" we can use to simplify
// and reduce the tree of mathematical operations.
namespace wf::ir {

// Add together two operands.
class add {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static std::string_view to_string() noexcept { return "add"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const add&) const noexcept { return true; }
};

// Add together N operands.
class addn {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr static std::string_view to_string() noexcept { return "addn"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const addn&) const noexcept { return true; }
};

// Cast the operand to the specified destination type.
class cast {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr static std::string_view to_string() noexcept { return "cast"; }
  constexpr std::size_t hash() const noexcept {
    return static_cast<std::size_t>(destination_type_);
  }
  constexpr bool is_identical_to(const cast& other) const noexcept {
    return destination_type_ == other.destination_type_;
  }

  // Construct w/ destination type.
  constexpr explicit cast(const code_numeric_type destination) noexcept
      : destination_type_(destination) {}

  // Access the target type we are casting to.
  constexpr code_numeric_type destination_type() const noexcept { return destination_type_; }

 private:
  code_numeric_type destination_type_;
};

// Call a custom user-provided function.
class call_external_function {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr static std::string_view to_string() noexcept { return "call"; }
  std::size_t hash() const noexcept { return function_.hash(); }
  bool is_identical_to(const call_external_function& other) const noexcept {
    return are_identical(function_, other.function_);
  }

  explicit call_external_function(external_function function) noexcept
      : function_(std::move(function)) {}

  // The user-specified external we are calling.
  constexpr const external_function& function() const noexcept { return function_; }

  // Return type of the external function.
  const type_variant& return_type() const noexcept { return function_.return_type(); }

 private:
  external_function function_;
};

// A call to a built-in mathematical function like sin, cos, log, etc.
class call_std_function {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr std::string_view to_string() const noexcept {
    return string_from_standard_library_function(name_);
  }
  constexpr std::size_t hash() const noexcept { return static_cast<std::size_t>(name_); }
  constexpr bool is_identical_to(const call_std_function& other) const noexcept {
    return name_ == other.name_;
  }

  explicit constexpr call_std_function(const std_math_function name) noexcept : name_(name) {}

  constexpr std_math_function name() const noexcept { return name_; }

 private:
  std_math_function name_;
};

// Compare two operands (equality or inequality).
class compare {
 public:
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }

  constexpr std::string_view to_string() const noexcept {
    switch (operation_) {
      case relational_operation::less_than:
        return "lt";
      case relational_operation::less_than_or_equal:
        return "lte";
      case relational_operation::equal:
        return "eq";
    }
    return "<NOT A VALID ENUM VALUE>";
  }

  constexpr std::size_t hash() const noexcept { return static_cast<std::size_t>(operation_); }
  constexpr bool is_identical_to(const compare& comp) const noexcept {
    return operation_ == comp.operation_;
  }

  explicit constexpr compare(relational_operation operation) noexcept : operation_(operation) {}

  // Access operation enum.
  constexpr relational_operation operation() const noexcept { return operation_; }

 private:
  relational_operation operation_;
};

// A trinary we use prior to insertion of conditional logic:
// cond(a, b, c) = a ? b : c;
// This expression is used to simplify duplicate elimination. It is then replaced by conditional
// logic and phi functions.
class cond {
 public:
  constexpr static int num_value_operands() noexcept { return 3; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static std::string_view to_string() noexcept { return "cond"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const cond&) const noexcept { return true; }
};

// Construct an aggregate type.
class construct {
 public:
  using variant_type = std::variant<matrix_type, custom_type>;

  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static std::string_view to_string() noexcept { return "new"; }

  std::size_t hash() const noexcept { return hash_variant<variant_type>{}(type_); }
  bool is_identical_to(const construct& other) const noexcept {
    return is_identical_variant<variant_type>{}(type_, other.type_);
  }

  constexpr const auto& type() const noexcept { return type_; }

  explicit construct(variant_type type) noexcept : type_(std::move(type)) {}

 private:
  variant_type type_;
};

// Copy the operand.
class copy {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr static std::string_view to_string() noexcept { return "copy"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const copy&) const noexcept { return true; }
};

// Divide first operand by the second one.
class div {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static std::string_view to_string() noexcept { return "div"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const div&) const noexcept { return true; }
};

// Get the n't element from a matrix or compound expression.
class get {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr static std::string_view to_string() noexcept { return "get"; }
  constexpr std::size_t hash() const noexcept { return index_; }
  constexpr bool is_identical_to(const get& other) const noexcept { return index_ == other.index_; }

  explicit constexpr get(const std::size_t index) noexcept : index_(index) {}

  // The element to access.
  constexpr std::size_t index() const noexcept { return index_; }

 private:
  std::size_t index_;
};

// This objects acts as a sink to indicate that a value will be used in the output code
// to adjust control flow. The single operand is a boolean value that will ultimately
// determine which path an if-else statement takes.
class jump_condition {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr static std::string_view to_string() noexcept { return "jcnd"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const jump_condition&) const noexcept { return true; }
};

// A source to insert input values (either constants or function arguments) into the IR. Has no
// value arguments, but has an expression.
class load {
 public:
  using storage_type =
      std::variant<symbolic_constant, integer_constant, float_constant, rational_constant,
                   boolean_constant, variable, custom_type_argument>;

  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr static std::string_view to_string() noexcept { return "load"; }

  std::size_t hash() const noexcept { return hash_variant<storage_type>{}(variant_); }

  //  Check if the underlying variants contain identical expressions.
  bool is_identical_to(const load& other) const {
    return is_identical_variant<storage_type>{}(variant_, other.variant_);
  }

  // Returns true if variant contains one of the types `Ts...`
  template <typename... Ts>
  bool is_type() const noexcept {
    return ((std::holds_alternative<Ts>(variant_)) || ...);
  }

  // Construct with one of the types in `storage_type`.
  explicit load(storage_type contents) : variant_{std::move(contents)} {}

  // Access the underlying variant of input expressions.
  constexpr const storage_type& variant() const noexcept { return variant_; }

 private:
  // Variant of different leaf expression types.
  storage_type variant_;
};

// Multiply together two operands.
class mul {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static std::string_view to_string() noexcept { return "mul"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const mul&) const noexcept { return true; }
};

// Multiply together N operands.
class muln {
 public:
  constexpr static bool is_commutative() noexcept { return true; }
  constexpr static int num_value_operands() noexcept { return -1; }
  constexpr static std::string_view to_string() noexcept { return "muln"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const muln&) const noexcept { return true; }
};

// Negate the operand.
class neg {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 1; }
  constexpr static std::string_view to_string() noexcept { return "neg"; };
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr bool is_identical_to(const neg&) const noexcept { return true; }
};

// Evaluates to true if the specified output index is required.
class output_required {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept { return 0; }
  constexpr static std::string_view to_string() noexcept { return "oreq"; }
  std::size_t hash() const noexcept { return hash_string_fnv(name_); }
  bool is_identical_to(const output_required& other) const noexcept { return name_ == other.name_; }

  // Construct with string name of the relevant optional output argument.
  explicit output_required(std::string name) : name_(std::move(name)) {}

  // Name of the output argument.
  constexpr const std::string& name() const noexcept { return name_; }

 private:
  std::string name_;
};

// Phi function. The output is equal to whichever operand was generated on the evaluated code-path.
class phi {
 public:
  constexpr static int num_value_operands() noexcept { return 2; }
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static std::string_view to_string() noexcept { return "phi"; }
  constexpr static std::size_t hash() noexcept { return 0; }
  constexpr static bool is_identical_to(const phi&) noexcept { return true; }
};

// A sink used to indicate that a value is consumed by the output (for example in a return type or
// an output argument). Operands to `save` are never eliminated.
class save {
 public:
  constexpr static bool is_commutative() noexcept { return false; }
  constexpr static int num_value_operands() noexcept {
    return -1;  //  Dynamic
  }
  constexpr static std::string_view to_string() noexcept { return "save"; }

  std::size_t hash() const noexcept { return hash_struct<output_key>{}(key_); }
  bool is_identical_to(const save& other) const noexcept { return key_ == other.key_; }

  // Construct with key.
  explicit save(output_key key) noexcept : key_(std::move(key)) {}

  // Get output key.
  constexpr const output_key& key() const noexcept { return key_; }

 private:
  output_key key_;
};

// Different operations are represented by a variant.
using operation = std::variant<add, addn, call_external_function, call_std_function, cast, compare,
                               cond, copy, construct, div, get, jump_condition, load, mul, muln,
                               neg, output_required, phi, save>;

// Used to indicate values that have no type.
struct void_type {};

}  // namespace wf::ir

// Trait implementations for hashing and testing equality.
namespace wf {
namespace detail {
template <typename T, typename Variant>
struct enable_if_member_of_variant;
template <typename T, typename... Ts>
struct enable_if_member_of_variant<T, std::variant<Ts...>>
    : std::enable_if<type_list_contains_v<T, type_list<Ts...>>> {};
template <typename T, typename Variant>
using enable_if_member_of_variant_t = typename enable_if_member_of_variant<T, Variant>::type;
}  // namespace detail

// We allow the definitions of hash+identity test logic to live on the struct definition, and
// invoke those methods here in the trait implementation. The rational is that it is easier for
// the implementation to live beside the member declarations.
template <typename T>
struct hash_struct<T, detail::enable_if_member_of_variant_t<T, ir::operation>> {
  std::size_t operator()(const T& thing) const noexcept { return thing.hash(); }
};

template <typename T>
struct is_identical_struct<T, detail::enable_if_member_of_variant_t<T, ir::operation>> {
  bool operator()(const T& a, const T& b) const noexcept { return a.is_identical_to(b); }
};

// Hashing of ir::operation.
template <>
struct hash_struct<ir::operation> : hash_variant<ir::operation> {};

// Identity test of ir::operation.
template <>
struct is_identical_struct<ir::operation> : is_identical_variant<ir::operation> {};

}  // namespace wf

// Formatter for void_type:
template <>
struct fmt::formatter<wf::ir::void_type, char> {
  constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(wf::ir::void_type, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "void");
  }
};
