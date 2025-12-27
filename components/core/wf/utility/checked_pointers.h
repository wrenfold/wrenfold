// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <system_error>  // for std::hash

#include "wf/utility/assertions.h"

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
    noexcept(detail::value_or_reference_return_t<T>{std::declval<T&&>()});

// True if casting `T` to bool is noexcept.
template <typename T>
constexpr bool is_nothrow_castable_to_bool_v =
    noexcept(static_cast<bool>(std::declval<const T&>()));
}  // namespace detail

// `non_null` wraps a pointer-like object `T` and checks on construction that it is not null. `T`
// could be a bare-pointer, or a shared_ptr or unique_ptr.
//
// This implementation is based on the GSL implementation, with some alterations - for example we do
// not allow implicit cast to the underlying type `T`.
template <typename T>
class non_null {
 public:
  template <typename U>
  using enable_if_convertible_t =
      std::enable_if_t<!std::is_same_v<U, T> && std::is_convertible_v<U, T>>;

  // Construct from type `U` that is convertible to `T`.
  // We check `ptr` for nullity, hence this constructor is always noexcept(false).
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr non_null(U&& ptr) noexcept(false) : ptr_(std::forward<U>(ptr)) {  // NOLINT
    WF_ASSERT(ptr_ != nullptr, "Cannot be constructed null");
  }

  // Construct from type `T`.
  // We check `ptr` for nullity, hence this constructor is always noexcept(false).
  constexpr non_null(T ptr) noexcept(false) : ptr_(std::move(ptr)) {  // NOLINT
    WF_ASSERT(ptr_ != nullptr, "Cannot be constructed null");
  }

  // Construct from another `non_null` type that is convertible.
  // We don't check here, because the constructor of `non_null<U>` already checked.
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr non_null(non_null<U> ptr) noexcept(std::is_nothrow_constructible_v<T, U&&>)  // NOLINT
      : ptr_(std::move(ptr).get_rvalue()) {}

  // Copy and move constructors.
  non_null(const non_null& other) = default;
  non_null& operator=(const non_null& other) = default;
  non_null(non_null&& other) = default;
  non_null& operator=(non_null&& other) = default;

  // nullptr constructor is explicitly deleted.
  [[maybe_unused]] non_null(std::nullptr_t) = delete;

#ifdef WF_CHECK_NON_NULL_POINTERS
  constexpr detail::value_or_reference_return_t<T> get() const {
    WF_ASSERT(ptr_, "Accessed moved-from pointer.");
    return ptr_;
  }
  constexpr decltype(auto) operator->() const { return get(); }
  constexpr decltype(auto) operator*() const { return *get(); }
#else
  // Access the underlying pointer.
  constexpr detail::value_or_reference_return_t<T> get() const noexcept { return ptr_; }

  // De-reference operators.
  constexpr decltype(auto) operator->() const noexcept { return get(); }
  constexpr decltype(auto) operator*() const noexcept { return *get(); }
#endif

  // Check underlying ptr is null. This can happen if we moved the underlying pointer out.
  constexpr explicit operator bool() const noexcept { return ptr_ != nullptr; }

 private:
  // Get r-value reference.
  constexpr decltype(auto) get_rvalue() && { return std::move(ptr_); }

  template <typename U>
  friend class non_null;

  T ptr_;
};

// Test for inequality.
// We need to explicitly implement nullptr_t comparison, since the implicit construtor is deleted.
template <typename T, typename U>
constexpr bool operator!=(const non_null<T>& a,
                          const non_null<U>& b) noexcept(noexcept(a.get() != b.get())) {
  return a.get() != b.get();
}
template <typename T>
constexpr bool operator!=(const non_null<T>& a,
                          std::nullptr_t) noexcept(noexcept(a.get() != nullptr)) {
  return a.get() != nullptr;
}
template <typename T>
constexpr bool operator!=(std::nullptr_t,
                          const non_null<T>& b) noexcept(noexcept(b.get() != nullptr)) {
  return b.get() != nullptr;
}

// Test for equality.
template <typename T, typename U>
constexpr bool operator==(const non_null<T>& a,
                          const non_null<U>& b) noexcept(noexcept(a.get() == b.get())) {
  return a.get() == b.get();
}
template <typename T>
constexpr bool operator==(const non_null<T>& a,
                          std::nullptr_t) noexcept(noexcept(!operator==(a, nullptr))) {
  return !operator==(a, nullptr);
}
template <typename T>
constexpr bool operator==(std::nullptr_t,
                          const non_null<T>& b) noexcept(noexcept(!operator==(b, nullptr))) {
  return !operator==(b, nullptr);
}

// `maybe_null` wraps a pointer-like object `T` and checks for nullity whenever the object is
// accessed via `get()` or the de-reference operators (* and ->).
template <typename T>
class maybe_null {
 public:
  static_assert(std::is_constructible_v<T, std::nullptr_t>, "T must be constructible from nullptr");

  template <typename U>
  using enable_if_convertible_t =
      std::enable_if_t<!std::is_same_v<T, U> && std::is_convertible_v<U, T>>;

  // Construct from type `U` that is convertible to `T`.
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr maybe_null(U&& ptr) noexcept(std::is_nothrow_constructible_v<T, decltype(ptr)>)
      : ptr_(std::forward<U>(ptr)) {}

  // Construct from type `T`.
  constexpr maybe_null(T ptr) noexcept(std::is_nothrow_move_constructible_v<T>)
      : ptr_(std::move(ptr)) {}

  // Construct from a other `maybe_null` type that is convertible.
  template <typename U, typename = enable_if_convertible_t<U>>
  constexpr maybe_null(maybe_null<U> ptr) noexcept(std::is_nothrow_constructible_v<T, U&&>)
      : ptr_(std::move(ptr).get_rvalue()) {}

  // Construct empty/null.
  constexpr maybe_null(std::nullptr_t) noexcept(std::is_nothrow_constructible_v<T, std::nullptr_t>)
      : ptr_(nullptr) {}

  maybe_null(const maybe_null& other) = default;
  maybe_null& operator=(const maybe_null& other) = default;

  maybe_null(maybe_null&& other) = default;
  maybe_null& operator=(maybe_null&& other) = default;

  // Check truthiness.
  constexpr explicit operator bool() const noexcept(detail::is_nothrow_castable_to_bool_v<T>) {
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

  // Access the underlying object safely. Throws if the underlying ptr is null. Hence, this is
  // always noexcept(false), because we check at runtime for a valid access.
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

  constexpr decltype(auto) get_rvalue() && { return std::move(ptr_); }

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

// True if the specified type is a maybe_null or non_null.
template <typename T>
struct is_checked_pointer : std::false_type {};
template <typename T>
struct is_checked_pointer<maybe_null<T>> : std::true_type {};
template <typename T>
struct is_checked_pointer<non_null<T>> : std::true_type {};

}  // namespace wf

// Specialization of std::hash for non_null
template <class T>
struct std::hash<wf::non_null<T>> {
  std::size_t operator()(const wf::non_null<T>& ptr) const
      noexcept(wf::detail::is_nothrow_convertible_to_value_or_ref_v<T>) {
    static_assert(std::is_default_constructible_v<std::hash<T>>,
                  "std::hash is not default constructible for underlying type in non_null object.");
    return std::hash<T>{}(ptr.get());
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
