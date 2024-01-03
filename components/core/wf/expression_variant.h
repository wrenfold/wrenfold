// Copyright 2024 Gareth Cross
#pragma once
#include <memory>

#include "wf/hashing.h"
#include "wf/type_list.h"

namespace wf {

template <typename T>
struct type_list_trait;

// Evaluate to true if visitor `F` can be nothrow invoked on all the possible contained types.
template <typename F, typename T>
struct is_nothrow_invokable_visitor;
template <typename F, typename... Ts>
struct is_nothrow_invokable_visitor<F, type_list<Ts...>>
    : std::conjunction<std::is_nothrow_invocable<F, const Ts&...>> {};
template <typename F, typename List>
constexpr bool is_nothrow_invokable_visitor_v = is_nothrow_invokable_visitor<F, List>::value;

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
    return ptr_->is_identical_to(*other.ptr_);
  }

  // Visit the stored value with the provided visitor object.
  // The visitor will be passed a const reference.
  template <typename F>
  auto visit(F&& f) const noexcept(is_nothrow_invokable_visitor_v<decltype(f), types>) {
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

    concept_base(std::size_t index, std::size_t hash) noexcept : hash_(hash), index_(index) {}

    constexpr std::size_t index() const noexcept { return index_; }
    constexpr std::size_t hash() const noexcept { return hash_; }

    virtual bool is_identical_to(const concept_base& other) const = 0;

   private:
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

    constexpr const value_type& contents() const noexcept { return contents_; }
    constexpr value_type& constents() noexcept { return contents_; }

    bool is_identical_to(const concept_base& other) const override final {
      if (concept_base::index() != other.index()) {
        return false;
      }
      return contents_.is_identical_to(static_cast<const model&>(other).contents_);
    }

   private:
    value_type contents_;
  };

  using concept_shared_ptr = std::shared_ptr<concept_base>;

  // Cast to const-reference of type `T`.
  template <typename T>
  __attribute__((always_inline)) const T& cast_to_type() const noexcept {
    const expression_variant::model<T>* model =
        static_cast<const expression_variant::model<T>*>(ptr_.get());
    return model->contents();
  }

  // Cast to const-reference of the type at index `I` in list `types`.
  template <std::size_t I>
  const auto& cast_to_index() const noexcept {
    static_assert(I < type_list_size_v<types>, "Index exceeds number of types");
    using type = type_list_element_t<I, types>;
    return cast_to_type<type>();
  }

  // If index `I` matches the internal index, call function `f` on it - otherwise recurse to the
  // next index.
  template <std::size_t I, typename F>
  __attribute__((always_inline)) auto visit_impl(F&& f) const
      noexcept(is_nothrow_invokable_visitor_v<decltype(f), types>) {
    if (index() == I) {
      return f(cast_to_index<I>());
    } else if constexpr (I < type_list_size_v<types> - 1) {
      return visit_impl<I + 1>(std::forward<F>(f));
    } else {
      return f(cast_to_index<type_list_size_v<types> - 1>());
    }
  }

  // Index + hash of the type.
  concept_shared_ptr ptr_;
};

}  // namespace wf
