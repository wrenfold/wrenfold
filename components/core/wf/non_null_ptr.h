// Copyright 2023 Gareth Cross
#pragma once
#include <system_error>  // for std::hash

#include "wf/assertions.h"

namespace wf {

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

}  // namespace wf

// Specialization of std::hash
namespace std {
template <class T>
struct hash<wf::non_null_ptr<T>> {
  std::size_t operator()(const wf::non_null_ptr<T>& ptr) const noexcept {
    return std::hash<const T*>{}(ptr.get());
  }
};
}  // namespace std
