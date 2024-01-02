#pragma once
#include <typeindex>
#include <unordered_map>

#include "wf/code_generation/types.h"
#include "wf/expressions/matrix.h"
#include "wf/output_annotations.h"
#include "wf/template_utils.h"
#include "wf/type_annotations.h"

namespace wf {
// We specialize this object on a type `T` so the code-generation framework understands
// how to map variables into the type.
template <typename T, typename = void>
struct custom_type_registrant;

// Store a mapping from `std::type_index` to `custom_type` object. Used to map from native C++ types
// to their `custom_type` description, which is used for code-generation.
class custom_type_registry {
 public:
  // Find a particular custom type (or none if it is not registered yet).
  template <typename T>
  std::optional<custom_type> find_type() const {
    if (const auto it = types_.find(typeid(T)); it != types_.end()) {
      return it->second;
    }
    return std::nullopt;
  }

  // Insert `custom_type` corresponding to native type `T`.
  template <typename T>
  void insert(const custom_type& type) {
    const auto [it, was_inserted] = types_.emplace(typeid(T), type);
    WF_ASSERT(was_inserted, "Attemped to insert duplicate type description for type `{}` (T = {})",
              type.name(), typeid(T).name());
  }

 private:
  std::unordered_map<std::type_index, custom_type> types_;
};

// Support setting and getting an anonymized field on type `T`.
// The type and name of the underlying member is erased (it is implemented in the derived class).
template <typename T>
class native_field_accessor_typed : public native_field_accessor::concept {
 public:
  // Set underlying member by consuming expressions from `input`.
  // A new truncated span with remaining elements is returned. It is assumed that expressions in
  // `input` can be moved.
  virtual absl::Span<Expr> set(T& object, absl::Span<Expr> input) const = 0;

  // Copy underlying member into `output` vector.
  virtual void get(const T& object, std::vector<Expr>& output) const = 0;
};

// Implement `native_field_accessor_typed` by using a member-variable pointer.
template <typename StructType, typename FieldType, typename U>
class native_field_accessor_member_ptr final : public native_field_accessor_typed<StructType> {
 public:
  static_assert(std::is_constructible_v<type_variant, U>,
                "Type U must be something that we can pass to type_variant.");

  explicit native_field_accessor_member_ptr(FieldType StructType::*member_ptr,
                                            U member_type) noexcept
      : member_ptr_(member_ptr), member_type_(std::move(member_type)) {}

  absl::Span<Expr> set(StructType& object, absl::Span<Expr> input) const override;
  void get(const StructType& object, std::vector<Expr>& output) const override;

 private:
  FieldType StructType::*member_ptr_;
  U member_type_;
};

namespace detail {
// The purpose of record_type is to convert from actual C++ types to our runtime representations.
// For example, Expr -> scalar_type, MatrixExpr -> matrix_type.
// We use this to scrape a c++ function signature and record type information for code-generation.
template <typename T, typename = void>
struct record_type;
}  // namespace detail

// Helper used to construct `custom_type` object for type `T`. The user is expect to specialize
// `custom_type_registrant<T>`. `custom_type_registrant<T>::operator()` should return an instance
// of `custom_type_builder<T>`.
//
// Example:
//  struct Foo {
//    Expr x;
//    Expr y;
//  };
//
//  struct custom_type_registrant<Foo> {
//    auto operator()(custom_type_registry& registry) const {
//      return custom_type_builder<Foo>(registry, "Foo")
//        .add_field("x", &Foo::x)
//        .add_field("y", &Foo::y);
//    }
//  }
template <typename T>
class custom_type_builder {
 public:
  custom_type_builder(custom_type_registry& registry, std::string name) noexcept
      : registry_(registry), name_(std::move(name)) {}

  // Add a field of type `P` with the provided name and member pointer.
  template <typename P>
  custom_type_builder& add_field(std::string name, P T::*member_ptr);

 protected:
  // Consume the contents of this object, build a custom_type, and put it in the registry.
  custom_type finalize() {
    std::type_index type_index = typeid(T);
    custom_type custom{std::move(name_), std::move(fields_), type_index};
    registry_.insert<T>(custom);
    return custom;
  }

  // So that record_type<T> can call finalize().
  friend struct detail::record_type<T>;

  custom_type_registry& registry_;
  std::string name_;
  std::vector<struct_field> fields_;
};

// Evaluates to true if there is a specialization of custom_type_registrant for the type T.
template <typename T>
constexpr bool implements_custom_type_registrant_v =
    has_call_operator_v<custom_type_registrant<T>, custom_type_registry&>;

// `custom_type` wrapped with a template that marks which C++ type it corresponds to.
template <typename T>
struct annotated_custom_type {
  static_assert(std::is_default_constructible_v<T>, "Custom types must be default constructible");

  custom_type type;

  // Create type `T` by initializing all registered members of `T` with expressions.
  // The trimmed span (after consuming the right # of values from the front) is returned.
  std::tuple<T, absl::Span<Expr>> initialize_from_expressions(absl::Span<Expr> expressions) const {
    T result{};
    for (const struct_field& field : type.fields()) {
      expressions =
          field.native_accessor().as<native_field_accessor_typed<T>>().set(result, expressions);
    }
    return std::make_tuple(std::move(result), expressions);
  }

  // Copy all fields from object of type `T` into vector `output`.
  void copy_output_expressions(const T& object, std::vector<Expr>& output) const {
    output.reserve(output.size() + type.size());
    for (const struct_field& field : type.fields()) {
      field.native_accessor().as<native_field_accessor_typed<T>>().get(object, output);
    }
  }

  // Allow implicit cast so we can construct type_variant from this.
  // ReSharper disable once CppNonExplicitConversionOperator
  operator type_variant() const noexcept { return type; }  // NOLINT
};

namespace detail {

template <>
struct record_type<Expr> {
  scalar_type operator()(const custom_type_registry&) const {
    return scalar_type(code_numeric_type::floating_point);
  }
};

template <index_t Rows, index_t Cols>
struct record_type<type_annotations::static_matrix<Rows, Cols>> {
  matrix_type operator()(const custom_type_registry&) const { return matrix_type(Rows, Cols); }
};

// For custom types, we check add each type to the type registry.
template <typename T>
struct record_type<T, std::enable_if_t<implements_custom_type_registrant_v<T>>> {
  annotated_custom_type<T> operator()(custom_type_registry& registry) const {
    // Check if this type has already been registered:
    if (std::optional<custom_type> existing_type = registry.find_type<T>();
        existing_type.has_value()) {
      return annotated_custom_type<T>{std::move(*existing_type)};
    }
    // The registrant should return `custom_type_builder<T>`:
    custom_type type = custom_type_registrant<T>{}(registry).finalize();
    return annotated_custom_type<T>{std::move(type)};
  }
};

// Specialization so we can opreate on return_value + output_arg.
template <typename T>
struct record_type<T, std::enable_if_t<is_output_arg_or_return_value<T>::value>> {
  auto operator()(custom_type_registry& registry) const {
    return record_type<typename T::value_type>{}(registry);
  }
};

// Invoke `record_types` on every type in `Ts...`.
// Returns a tuple of [scalar_type, matrix_type, custom_type].
// Custom types are placed in the custom_type_registry.
template <typename... Ts>
auto record_arg_types(custom_type_registry& registry, type_list<Ts...>) {
  return std::make_tuple(record_type<std::decay_t<Ts>>{}(registry)...);
}

// Create scalar input required to evalute a symbolic function.
Expr create_function_input(const scalar_type& scalar, std::size_t arg_index);

// Create matrix input required to evalute a symbolic function.
MatrixExpr create_function_input(const matrix_type& mat, std::size_t arg_index);

// Determine the size of a custom type, and create enough variables to fill it.
std::vector<Expr> create_function_input(const custom_type& custom, std::size_t arg_index);

// Fill a custom type `T` with symbolic variable expressions.
template <typename T>
T create_function_input(const annotated_custom_type<T>& custom, std::size_t arg_index) {
  std::vector<Expr> expressions = create_function_input(custom.type, arg_index);
  auto [instance, _] = custom.initialize_from_expressions(absl::Span<Expr>{expressions});
  return instance;
}

// Copy output from scalar expression into a vector.
std::vector<Expr> extract_function_output(const scalar_type& scalar, const Expr& value);

// Copy output from matrix expression into a vector.
std::vector<Expr> extract_function_output(const matrix_type& mat, const MatrixExpr& value);

// Copy output from a custom struct into a vector.
template <typename T>
std::vector<Expr> extract_function_output(const annotated_custom_type<T>& custom,
                                          const T& instance) {
  std::vector<Expr> result;
  custom.copy_output_expressions(instance, result);
  return result;
}

}  // namespace detail

template <typename T>
template <typename P>
custom_type_builder<T>& custom_type_builder<T>::add_field(std::string name, P T::*member_ptr) {
  static_assert(has_call_operator_v<detail::record_type<P>, custom_type_registry&>,
                "The specified type is not something we understand. Maybe you need to implement "
                "custom_type_registrant.");
  WF_ASSERT(member_ptr != nullptr);

  // Convert the type `P` to something we can place inside `type_variant`.
  auto member_type = detail::record_type<P>{}(registry_);
  using member_type_type = std::remove_const_t<decltype(member_type)>;

  // Save the name and type of this member so we can build `custom_type` in finalize().
  // Storing `member_type` twice here is a bit wasteful maybe, but simplifies implementation
  // of field accessor.
  native_field_accessor field_accessor{
      native_field_accessor_member_ptr<T, P, member_type_type>{member_ptr, member_type}};
  fields_.emplace_back(std::move(name), type_variant{member_type}, std::move(field_accessor));

  return *this;
}

template <typename StructType, typename FieldType, typename U>
absl::Span<Expr> native_field_accessor_member_ptr<StructType, FieldType, U>::set(
    StructType& object, absl::Span<Expr> input) const {
  if constexpr (std::is_same_v<U, scalar_type>) {
    WF_ASSERT(!input.empty());
    object.*member_ptr_ = std::move(input.front());
    return input.subspan(1);
  } else if constexpr (std::is_same_v<U, matrix_type>) {
    const matrix_type& mat = member_type_;
    WF_ASSERT_GREATER_OR_EQ(input.size(), mat.size());
    const auto begin = std::make_move_iterator(input.begin());
    object.*member_ptr_ = MatrixExpr::create(mat.rows(), mat.cols(), begin, begin + mat.size());
    return input.subspan(mat.size());
  } else {
    auto [instance, output_span] = member_type_.initialize_from_expressions(input);
    object.*member_ptr_ = std::move(instance);
    return output_span;
  }
}

template <typename StructType, typename FieldType, typename U>
void native_field_accessor_member_ptr<StructType, FieldType, U>::get(
    const StructType& object, std::vector<Expr>& output) const {
  if constexpr (std::is_same_v<U, scalar_type>) {
    output.push_back(object.*member_ptr_);
  } else if constexpr (std::is_same_v<U, matrix_type>) {
    const matrix& mat = static_cast<const MatrixExpr&>(object.*member_ptr_).as_matrix();
    output.insert(output.end(), mat.begin(), mat.end());
  } else {
    // annotated_custom_type
    member_type_.copy_output_expressions(object.*member_ptr_, output);
  }
}

}  // namespace wf
