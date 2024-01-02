// Copyright 2024 Gareth Cross
#pragma once
#include "wf/type_list.h"

#include <limits>
#include <memory>
#include <typeindex>

namespace wf {

template <typename T>
struct poly_meta;

//
template <typename Meta>
class polymorphic {
 public:
  using trivial_types = typename poly_meta<Meta>::trivial_types;
  using non_trivial_types = typename poly_meta<Meta>::non_trivial_types;
  using all_types = type_list_concatenate_t<trivial_types, non_trivial_types>;

  // The number of types in `TrivialTypes`.
  static constexpr std::size_t num_trivial_types = type_list_size_v<trivial_types>;

  // Index used to represent a valueless object that has been moved from.
  static constexpr std::size_t valueless_index = std::numeric_limits<std::size_t>::max();

  template <typename T, typename List>
  using enable_if_type_in_list_t = enable_if_contains_type_t<std::decay_t<T>, List>;

  // Copy or move construct from an instance of type `T`.
  template <typename T, typename = enable_if_type_in_list_t<T, all_types>>
  polymorphic(T&& value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, decltype(value)>)
      : index_{type_list_index_v<std::decay_t<T>, all_types>} {
    using decayed_type = std::decay_t<T>;  //  remove const and ref
    if constexpr (type_list_contains_v<decayed_type, trivial_types>) {
      new (&data_) decayed_type(std::forward<T>(value));
    } else if constexpr (type_list_contains_v<decayed_type, non_trivial_types>) {
      // Could throw bad_alloc here, but we don't care about that - let it terminate.
      // We care primarily about exceptions thrown from the constructor of `T`.
      auto shared_value = std::make_shared<model<decayed_type>>(std::forward<T>(value));
      new (&data_) concept_shared_ptr(std::move(shared_value));
    }
  }

  // Decrement reference count if the stored type is non-trivial.
  ~polymorphic() { destroy(); }

  // Copy-constructor.
  polymorphic(const polymorphic& source) noexcept { copy_from(source); }

  // Move-constructor.
  polymorphic(polymorphic&& source) noexcept { move_from(std::move(source)); }

  // Copy-assign.
  polymorphic& operator=(const polymorphic& source) noexcept {
    if (this == &source) {
      return *this;
    }
    destroy();
    copy_from(source);
    return *this;
  }

  // Move-assign.
  polymorphic& operator=(polymorphic&& source) noexcept {
    if (this == &source) {
      return *this;
    }
    destroy();
    move_from(std::move(source));
    return *this;
  }

  // Return index indicating which type is stored.
  constexpr std::size_t index() const noexcept { return index_; }

  // True if this is stack allocated trivial object.
  constexpr bool is_stack_allocated() const noexcept { return index_ < num_trivial_types; }

  // False if the object has been moved-from (valueless).
  constexpr bool has_value() const noexcept { return index_ != valueless_index; }

  // Check if the underlying derived type is one of `Ts...`.
  template <typename... Ts>
  constexpr bool is_type() const noexcept {
    static_assert((type_list_contains_v<Ts, all_types> && ...), "T is not a valid expression type");
    return ((type_list_index_v<Ts, all_types> == index_) || ...);
  }

  // Return the address of the underlying value.
  const void* get_address() const noexcept {
    if (is_stack_allocated()) {
      return static_cast<const void*>(&data_);
    }
    return static_cast<const void*>(as_shared_ptr().get());
  }

  // Visit the stored value with the provided visitor object.
  // The visitor will be passed a const reference.
  template <typename F>
  auto visit(F&& f) const noexcept(is_nothrow_invokable_visitor<decltype(f)>::value) {
    return visit_impl<0>(std::forward<F>(f));
  }

  template <typename T>
  const T& cast_unchecked() const noexcept {
    static_assert(type_list_contains_v<T, all_types>, "Not a valid type to cast to.");
    return cast_to_index<type_list_index_v<T, all_types>>();
  }

 protected:
  // Evaluate to true if visitor `F` can be nothrow invoked on all the possible contained types.
  template <typename F, typename T = all_types>
  struct is_nothrow_invokable_visitor;
  template <typename F, typename... Ts>
  struct is_nothrow_invokable_visitor<F, type_list<Ts...>>
      : std::conjunction<std::is_nothrow_invocable<F, const Ts&...>> {};

  // True if the specified type only consists of trivially copyable types.
  template <typename T>
  struct types_are_trivially_copyable;
  template <typename... Ts>
  struct types_are_trivially_copyable<type_list<Ts...>>
      : std::conjunction<std::is_trivially_copyable<Ts>...> {};

  // Check that everything in `TrivialTypes` is actually trivially copyable.
  static_assert(types_are_trivially_copyable<trivial_types>::value,
                "One of the provided types is not actually trivially copyable.");

  // Abstract base for types we store on the heap.
  class concept_base {
   public:
    virtual ~concept_base() = default;
  };

  // When using heap allocation, this object stores the underlying value.
  template <typename T>
  class model final : public concept_base {
   public:
    static_assert(!std::is_const_v<T> && !std::is_reference_v<T>,
                  "Should be a plain type with no qualification");
    using value_type = T;

    explicit model(value_type contents) noexcept(std::is_nothrow_move_constructible_v<value_type>)
        : contents_(std::move(contents)) {}

    constexpr const value_type& contents() const noexcept { return contents_; }
    constexpr value_type& constents() noexcept { return contents_; }

   private:
    value_type contents_;
  };

  using concept_shared_ptr = std::shared_ptr<concept_base>;

  // We are assuming shared_ptr is nothrow move and copy constructible.
  static_assert(std::is_nothrow_move_constructible_v<concept_shared_ptr> &&
                std::is_nothrow_copy_constructible_v<concept_shared_ptr>);

  // The list of types we'll place in our std::aligned_union:
  using storage_type_list = type_list_push_back_t<concept_shared_ptr, trivial_types>;

  // Do unchecked cast to concept_shared_ptr.
  // Safety: This is valid if a shared pointer was explicitly constructed at `&data_`.
  constexpr concept_shared_ptr& as_shared_ptr() noexcept {
    return *reinterpret_cast<concept_shared_ptr*>(&data_);
  }

  // Do unchecked cast to concept_shared_ptr.
  constexpr const concept_shared_ptr& as_shared_ptr() const noexcept {
    return *reinterpret_cast<const concept_shared_ptr*>(&data_);
  }

  // If the stored object is in a shared_ptr, invoke the shared_ptr destructor.
  // It is UB for an object inside a shared_ptr to throw, so this is noexcept.
  void destroy() noexcept {
    if (!is_stack_allocated() && has_value()) {
      as_shared_ptr().~concept_shared_ptr();
    }
  }

  // Copy from another instance.
  void copy_from(const polymorphic& source) noexcept {
    index_ = source.index_;
    if (is_stack_allocated()) {
      std::memcpy(&data_, &source.data_, sizeof(data_));
    } else if (has_value()) {
      new (&data_) concept_shared_ptr(source.as_shared_ptr());
    }
  }

  // Move from antother instance.
  void move_from(polymorphic&& source) noexcept {
    index_ = source.index_;
    source.index_ = valueless_index;  //  Mark as moved-from.
    if (is_stack_allocated()) {
      std::memcpy(&data_, &source.data_, sizeof(data_));
    } else if (has_value()) {
      // Move-construct shared_ptr: reference count stays the same.
      new (&data_) concept_shared_ptr(std::move(source.as_shared_ptr()));
    }
  }

  template <std::size_t I>
  const auto& cast_to_index() const {
    static_assert(I < type_list_size_v<all_types>, "Index exceeds number of types");
    using type = type_list_element_t<I, all_types>;
    if constexpr (I < num_trivial_types) {
      // Safety: We can reinterpret_cast here because `data_` points to an instance of `type`, which
      // was explicitly constructed at that address.
      return *reinterpret_cast<const type*>(&data_);
    } else {
      const polymorphic::model<type>* model =
          static_cast<const polymorphic::model<type>*>(as_shared_ptr().get());
      return model->contents();
    }
  }

  // If index `I` matches the internal index, call function `f` on it - otherwise
  // recurse to the next index.
  template <std::size_t I, typename F>
  auto visit_impl(F&& f) const {
    if (index_ == I) {
      return f(cast_to_index<I>());
    } else if constexpr (I < type_list_size_v<all_types> - 1) {
      return visit_impl<I + 1>(std::forward<F>(f));
    } else {
      return f(cast_to_index<type_list_size_v<all_types> - 1>());
    }
  }

  // Index of the type
  std::size_t index_;
  aligned_union_from_type_list_t<storage_type_list> data_;
};

}  // namespace wf
