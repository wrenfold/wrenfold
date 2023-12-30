// Copyright 2023 Gareth Cross
#pragma once
#include <system_error>  // for std::hash

#include "wf/assertions.h"

namespace wf {
namespace detail {
// Based on the GSL implementation, which is based on the CPP core guidelines:
template <typename T>
using value_or_reference_return_t =
    std::conditional_t<sizeof(T) < 2 * sizeof(void*) && std::is_trivially_copy_constructible_v<T>,
                       const T, const T&>;

// True if converting T to `value_or_reference_return_t<T>` is noexcept.
template <typename T>
constexpr bool is_nothrow_convertible_to_value_or_ref_v =
    noexcept(detail::value_or_reference_return_t<T>{std::declval<T&>()});

// True if casting `T` to bool is noexcept.
template <typename T>
constexpr bool is_nothrow_castable_to_bool_v =
    noexcept(static_cast<bool>(std::declval<const T&>()));
}  // namespace detail

// Simple checked pointer type which cannot be null.
template <typename T>
class non_null_ptr {
 public:
  explicit non_null_ptr(T* ptr) : ptr_(ptr) { WF_ASSERT(ptr_, "Cannot be constructed null"); }

  // Construct from unique_ptr.
  explicit non_null_ptr(const std::unique_ptr<T>& ptr) : non_null_ptr(ptr.get()) {}

  // Construct from non-const version of ourselves:
  template <typename U, typename = std::enable_if_t<std::is_const_v<T> &&
                                                    std::is_same_v<U, std::remove_const_t<T>>>>
  constexpr non_null_ptr(const non_null_ptr<U>& other) noexcept : ptr_(other.ptr) {}  // NOLINT

  // nullptr constructor is explicitly deleted.
  [[maybe_unused]] non_null_ptr(std::nullptr_t) = delete;

  // Access the underlying pointer.
  constexpr T* get() const noexcept { return ptr_; }

  // De-reference operators.
  constexpr decltype(auto) operator->() const noexcept { return get(); }
  constexpr decltype(auto) operator*() const noexcept { return *get(); }

  // Check if owning ptr still lives.
  constexpr operator bool() const noexcept { return ptr_ != nullptr; }

 private:
  T* ptr_;
};

// Test for equality.
template <typename T>
constexpr bool operator==(const non_null_ptr<T>& a, const non_null_ptr<T>& b) noexcept {
  return a.get() == b.get();
}
template <typename T>
constexpr bool operator==(const non_null_ptr<T>& a, const T* b) noexcept {
  return a.get() == b;
}
template <typename T>
constexpr bool operator==(const T* a, const non_null_ptr<T>& b) noexcept {
  return a == b.get();
}

// Test for inequality.
template <typename T>
constexpr bool operator!=(const non_null_ptr<T>& a, const non_null_ptr<T>& b) noexcept {
  return a.get() != b.get();
}
template <typename T>
constexpr bool operator!=(const non_null_ptr<T>& a, const T* b) noexcept {
  return a.get() != b;
}
template <typename T>
constexpr bool operator!=(const T* a, const non_null_ptr<T>& b) noexcept {
  return a != b.get();
}

// `maybe_null` wraps a pointer-like object `T` and checks for nullity whenever the object is
// accessed via `get()` or the de-reference operators (* and ->).
template <typename T>
class maybe_null {
 public:
  static_assert(std::is_constructible_v<T, std::nullptr_t>, "T must be constructible from nullptr");

  template <typename U>
  using enable_if_convertible_t = std::enable_if_t<std::is_convertible_v<U, T>>;

  // Construct from type `U` that is convertible to `T`.
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr maybe_null(U&& ptr) noexcept(std::is_nothrow_constructible_v<T, decltype(ptr)>)
      : ptr_(std::forward<U>(ptr)) {}

  // Construct from type `T`.
  template <typename = std::enable_if_t<!std::is_same_v<std::nullptr_t, T>>>
  constexpr maybe_null(T ptr) noexcept(std::is_nothrow_move_constructible_v<T>)
      : ptr_(std::move(ptr)) {}

  // Construct from a other `maybe_null` type that is convertible.
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr maybe_null(const maybe_null<U>& ptr) noexcept(std::is_nothrow_move_constructible_v<T>)
      : maybe_null(ptr.get()) {}

  // Construct empty/null.
  constexpr maybe_null(std::nullptr_t) noexcept(std::is_nothrow_constructible_v<T, std::nullptr_t>)
      : ptr_(nullptr) {}

  maybe_null(const maybe_null& other) noexcept(std::is_nothrow_copy_constructible_v<T>) = default;
  maybe_null& operator=(const maybe_null& other) noexcept(std::is_nothrow_copy_assignable_v<T>) =
      default;

  maybe_null(maybe_null&& other) noexcept(std::is_nothrow_move_constructible_v<T>) = default;
  maybe_null& operator=(maybe_null&& other) noexcept(std::is_nothrow_move_assignable_v<T>) =
      default;

  // Move construct non-const to const:
  template <typename U, typename = std::enable_if_t<std::is_same_v<T, const U>>>
  constexpr maybe_null(maybe_null<U> other) noexcept(std::is_nothrow_move_constructible_v<T>)
      : ptr_(std::move(other.ptr_)) {}

  // Check truthiness.
  constexpr operator bool() const noexcept(detail::is_nothrow_castable_to_bool_v<T>) {  //  NOLINT
    return static_cast<bool>(ptr_);
  }

  // Check truthiness.
  constexpr bool has_value() const noexcept(detail::is_nothrow_castable_to_bool_v<T>) {
    return static_cast<bool>(ptr_);
  }

  // Access without any check for null.
  constexpr detail::value_or_reference_return_t<T> get_unchecked() const
      noexcept(detail::is_nothrow_convertible_to_value_or_ref_v<T>) {
    return ptr_;
  }

  // Access the underlying object safely. Throws if the underlying ptr is null. Hence this is always
  // noexcept(false), because we check at runtime for a valid access.
  detail::value_or_reference_return_t<T> get() const noexcept(false) {
    WF_ASSERT(has_value(), "Accessing maybe_null that is null. T = {}", typeid(T).name());
    return ptr_;
  }

  // Pointer access operators. These both use `get()` and will check for null first.
  decltype(auto) operator->() const noexcept(false) { return get(); }
  decltype(auto) operator*() const noexcept(false) { return *get(); }

 private:
  template <typename U>
  friend class maybe_null;

  T ptr_;
};

template <class T, class U>
constexpr auto operator==(const maybe_null<T>& lhs,
                          const maybe_null<U>& rhs) noexcept(noexcept(lhs.get_unchecked() ==
                                                                      rhs.get_unchecked())) {
  return lhs.get_unchecked() == rhs.get_unchecked();
}

template <class T, class U>
constexpr auto operator!=(const maybe_null<T>& lhs,
                          const maybe_null<U>& rhs) noexcept(noexcept(lhs.get_unchecked() !=
                                                                      rhs.get_unchecked())) {
  return lhs.get_unchecked() != rhs.get_unchecked();
}

}  // namespace wf

// Specialization of std::hash for non_null_ptr
template <class T>
struct std::hash<wf::non_null_ptr<T>> {
  std::size_t operator()(const wf::non_null_ptr<T>& ptr) const noexcept {
    return std::hash<const T*>{}(ptr.get());
  }
};

// Specialization of std::hash for maybe_null
template <class T>
struct std::hash<wf::maybe_null<T>> {
  std::size_t operator()(const wf::maybe_null<T>& ptr) const
      noexcept(wf::detail::is_nothrow_convertible_to_value_or_ref_v<T>) {
    using U = decltype(ptr.get_unchecked());
    static_assert(
        std::is_default_constructible_v<std::hash<U>>,
        "std::hash is not default constructible for underlying type in maybe_null object.");
    return std::hash<U>{}(ptr.get_unchecked());
  }
};
