// Copyright 2024 Gareth Cross
#pragma once
#include <memory>

#include "wf/error_types.h"
#include "wf/hashing.h"
#include "wf/type_list.h"

namespace wf {

template <typename T>
struct type_list_trait;

// Evaluate to true if visitor `F` can be nothrow invoked on all the possible contained types.
template <typename F, typename T>
struct is_nothrow_invocable_visitor;
template <typename F, typename... Ts>
struct is_nothrow_invocable_visitor<F, type_list<Ts...>>
    : std::conjunction<std::is_nothrow_invocable<F, const Ts&...>> {};
template <typename F, typename List>
constexpr bool is_nothrow_invocable_visitor_v = is_nothrow_invocable_visitor<F, List>::value;

// Stores one of `N` possible expression types in a shared_ptr, along with an index indicating which
// underlying expression is stored. To avoid bloating the class signature in the debugger and in
// error messages, the list of contained types is passed as a meta type `Meta` that implements
// `type_list_trait<Meta>`.
template <typename Meta>
class expression_variant {
 public:
  using types = typename type_list_trait<Meta>::types;

  // Check if the decayed type `T` is in our list of supported types.
  template <typename T>
  using enable_if_is_constructible_t = enable_if_contains_type_t<std::decay_t<T>, types>;

  // Copy or move construct from an instance of type `T`.
  template <typename T, typename U = std::decay_t<T>, typename = enable_if_is_constructible_t<T>>
  explicit expression_variant(T&& value) noexcept(
      std::is_nothrow_constructible_v<U, decltype(value)>)
      : ptr_(std::make_shared<model<U>>(std::forward<T>(value))) {}

  // In-place construct type `T` from `Args`.
  template <typename T, typename... Args, typename = enable_if_is_constructible_t<T>>
  explicit expression_variant(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<T, decltype(args)...>)
      : ptr_(std::make_shared<model<T>>(std::in_place_t{}, std::forward<Args>(args)...)) {}

  // Return index indicating which type is stored.
  std::size_t index() const noexcept { return ptr_->index(); }

  // Return the hash value.
  std::size_t hash() const noexcept { return ptr_->hash(); }

  // False if the object has been moved-from (valueless).
  bool has_value() const noexcept { return static_cast<bool>(ptr_); }

  // Check if the underlying derived type is one of `Ts...`.
  template <typename... Ts>
  bool is_type() const noexcept {
    static_assert((type_list_contains_v<Ts, types> && ...), "T is not a valid expression type");
    return ((type_list_index_v<Ts, types> == index()) || ...);
  }

  // Return the address of the underlying value.
  const void* get_address() const noexcept { return static_cast<const void*>(ptr_.get()); }

  // Check if two expression variants contain identical expressions.
  bool is_identical_to(const expression_variant& other) const {
    if (ptr_.get() == other.ptr_.get()) {
      return true;
    }
    return index() == other.index() && ptr_->is_identical_to(*other.ptr_);
  }

  std::string_view type_name() const noexcept { return ptr_->type_name(); }

  // Visit the stored value with the provided visitor object.
  // The visitor will be passed a const reference.
  template <typename F>
  auto visit(F&& f) const noexcept(is_nothrow_invocable_visitor_v<decltype(f), types>) {
    return visit_impl<0>(std::forward<F>(f));
  }

  // Do an unchecked cast to type `T`. If the specified type is incorrect, UB will occur.
  template <typename T>
  const T& cast_unchecked() const noexcept {
    static_assert(type_list_contains_v<T, types>, "Not a valid type to cast to.");
    return cast_to_type<T>();
  }

 protected:
  // Base-type we place in a shared_ptr.
  class concept_base {
   public:
    virtual ~concept_base() = default;

    concept_base(const std::size_t index, const std::size_t hash) noexcept
        : hash_(hash), index_(index) {}

    // Hash is initialized later in this version of the constructor.
    // ReSharper disable once CppPossiblyUninitializedMember
    explicit concept_base(const std::size_t index) noexcept : index_(index) {}

    constexpr std::size_t index() const noexcept { return index_; }
    constexpr std::size_t hash() const noexcept { return hash_; }

    virtual bool is_identical_to(const concept_base& other) const = 0;

    // Get the name of the underlying type.
    virtual std::string_view type_name() const noexcept = 0;

   protected:
    std::size_t hash_;
    std::size_t index_;
  };

  // Concrete storage of the underlying value of type `T`.
  template <typename T>
  class model final : public concept_base {
   public:
    static_assert(!std::is_const_v<T> && !std::is_reference_v<T>,
                  "Should be a plain type with no qualification");
    using value_type = T;

    static constexpr std::size_t type_index = type_list_index_v<T, expression_variant::types>;

    // Copy/move construct.
    explicit model(value_type contents) noexcept(std::is_nothrow_move_constructible_v<value_type>)
        : concept_base(type_index, wf::hash_combine(type_index, wf::hash(contents))),
          contents_(std::move(contents)) {}

    // In-place construction.
    template <typename... Args>
    explicit model(std::in_place_t, Args&&... args) noexcept(
        std::is_nothrow_constructible_v<value_type, decltype(args)...>)
        : concept_base(type_index), contents_(std::forward<Args>(args)...) {
      // Need to first construct `T`, then the hash can be initialized here:
      concept_base::hash_ = wf::hash_combine(type_index, wf::hash(contents_));
    }

    constexpr const value_type& contents() const noexcept { return contents_; }
    constexpr value_type& contents() noexcept { return contents_; }

    bool is_identical_to(const concept_base& other) const override {
      if constexpr (std::is_invocable_v<is_identical_struct<T>, const T&, const T&>) {
        // TODO: Switch all types to this path (implement the trait).
        return is_identical_struct<T>{}(contents_, static_cast<const model&>(other).contents_);
      } else {
        // Unchecked cast, this was checked in expression_variant::is_identical_to
        return contents_.is_identical_to(static_cast<const model&>(other).contents_);
      }
    }

    std::string_view type_name() const noexcept override { return T::name_str; }

   private:
    value_type contents_;
  };

  using concept_shared_ptr = std::shared_ptr<concept_base>;

  // Cast to const-reference of type `T`.
  template <typename T>
  const T& cast_to_type() const noexcept {
    const expression_variant::model<T>* model =
        static_cast<const expression_variant::model<T>*>(ptr_.get());
    return model->contents();
  }

  // Cast to const-reference of the type at index `I` in list `types`.
  template <std::size_t I>
  const auto& cast_to_index() const noexcept {
    static_assert(I < type_list_size_v<types>, "Index exceeds number of types");
    return cast_to_type<type_list_element_t<I, types>>();
  }

  // If index `I` matches the internal index, call function `f` on it - otherwise recurse to the
  // next index.
  template <std::size_t I, typename F>
  auto visit_impl(F&& f) const noexcept(is_nothrow_invocable_visitor_v<decltype(f), types>) {
    if (index() == I) {
      return f(cast_to_index<I>());
    } else if constexpr (I + 1 < type_list_size_v<types>) {
      return visit_impl<I + 1>(std::forward<F>(f));
    } else {
      return f(cast_to_index<type_list_size_v<types> - 1>());
    }
  }

  // Index + hash of the type.
  concept_shared_ptr ptr_;
};

// Categories of expressions (scalar, matrix, etc) should inherit from this type with their
// specified type list. This class provides some convenience constructors and methods, as well as
// declaring the underlying storage. This is not intended to be a virtual base.
template <typename Derived, typename Meta>
class expression_base {
 public:
  using storage_type = expression_variant<Meta>;
  using types = typename storage_type::types;

  // Enable if `storage_type` support construction from type `T`.
  template <typename T>
  using enable_if_is_constructible_t =
      typename storage_type::template enable_if_is_constructible_t<T>;

  // Construct from anything that can be fed into `storage_type`.
  template <typename T, typename = enable_if_is_constructible_t<T>>
  explicit expression_base(T&& arg) noexcept(
      std::is_nothrow_constructible_v<storage_type, decltype(arg)>)
      : impl_(std::forward<T>(arg)) {}

  // In-place construction with type `T`:
  template <typename T, typename... Args, typename = enable_if_is_constructible_t<T>>
  explicit expression_base(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<storage_type, std::in_place_type_t<T>, decltype(args)...>)
      : impl_(std::in_place_type_t<T>{}, std::forward<Args>(args)...) {}

  // Construct from expression storage.
  explicit expression_base(storage_type contents) noexcept : impl_(std::move(contents)) {}

  // Test if the two expressions have the same underlying address.
  bool has_same_address(const expression_base& other) const noexcept {
    return impl_.get_address() == other.impl_.get_address();
  }

  // Test if the two expressions are identical.
  bool is_identical_to(const expression_base& other) const {
    return impl_.is_identical_to(other.impl_);
  }

  // Check if the underlying expression is one of the specified types.
  template <typename... Ts>
  bool is_type() const noexcept {
    return impl_.template is_type<Ts...>();
  }

  // Get the hash of the expression.
  std::size_t hash() const noexcept { return impl_.hash(); }

  // Access underlying expression variant.
  constexpr const storage_type& impl() const noexcept { return impl_; }

  // Return the unique index of the underlying type.
  std::size_t type_index() const noexcept { return impl_.index(); }

  // Return camel-case type name.
  std::string_view type_name() const noexcept { return impl_.type_name(); }

  // Cast to derived type.
  constexpr const Derived& as_derived() const noexcept {
    return static_cast<const Derived&>(*this);
  }
  constexpr Derived& as_derived() noexcept { return static_cast<Derived&>(*this); }

 protected:
  storage_type impl_;
};

namespace detail {
constexpr auto inherits_expression_base_(...) -> std::false_type;
template <typename Derived, typename Meta>
constexpr auto inherits_expression_base_(const expression_base<Derived, Meta>&) -> std::true_type;
}  // namespace detail

// Evaluates to std::true_type if `T` inherits from expression_base, otherwise std::false_type.
template <typename T>
using inherits_expression_base =
    decltype(detail::inherits_expression_base_(std::declval<const T>()));
template <typename T>
constexpr bool inherits_expression_base_v = inherits_expression_base<T>::value;

// Enable if `T` inherits from `expression_base`.
template <typename T, typename U = void>
using enable_if_inherits_expression_base_t = std::enable_if_t<inherits_expression_base_v<T>, U>;

// Enable hashing of all types that inherit from `expression_base`.
template <typename T>
struct hash_struct<T, enable_if_inherits_expression_base_t<T>> {
  std::size_t operator()(const T& object) const noexcept { return object.hash(); }
};

// Enable identical test on all types that inherit from `expression_base`.
template <typename T>
struct is_identical_struct<T, enable_if_inherits_expression_base_t<T>> {
  bool operator()(const T& a, const T& b) const { return a.is_identical_to(b); }
};

// Cast expression to const pointer of the specified type.
// Returned pointer is valid in scope only as long as the argument `x` survives.
template <typename T, typename D, typename M>
const T* cast_ptr(const expression_base<D, M>& x) noexcept {
  if (x.template is_type<T>()) {
    const T& concrete = x.impl().template cast_unchecked<T>();
    return &concrete;
  } else {
    return nullptr;
  }
}

// Cast expression to const reference of the specified type. TypeError is thrown if the cast is
// invalid.
template <typename T, typename D, typename M>
const T& cast_checked(const expression_base<D, M>& x) {
  if (x.template is_type<T>()) {
    const T& concrete = x.impl().template cast_unchecked<T>();
    return concrete;
  } else {
    throw type_error("Cannot cast expression of type `{}` to `{}`", x.type_name(), T::name_str);
  }
}

// Cast expression with no checking. UB will occur if the wrong type is accessed.
template <typename T, typename D, typename M>
const T& cast_unchecked(const expression_base<D, M>& x) noexcept {
  return x.impl().template cast_unchecked<T>();
}

}  // namespace wf
