// Copyright 2023 Gareth Cross
#include <system_error>  // for std::hash

namespace math {

// Simple checked pointer type which cannot be null.
template <typename T>
class NonNullPtr {
 public:
  explicit NonNullPtr(T* ptr) : ptr_(ptr) { ASSERT(ptr_, "Cannot be constructed null"); }

  // Construct from unique_ptr.
  explicit NonNullPtr(const std::unique_ptr<T>& ptr) : NonNullPtr(ptr.get()) {}

  // Construct from non-const version of ourselves:
  template <typename U, typename = std::enable_if_t<std::is_const_v<T> &&
                                                    std::is_same_v<U, std::remove_const_t<T>>>>
  NonNullPtr(const NonNullPtr<U>& other) : ptr_(other.ptr) {}

  // nullptr constructor is explicitly deleted.
  [[maybe_unused]] NonNullPtr(std::nullptr_t) = delete;

  // Access the underlying pointer, w/ a check for null first.
  T* get() const {
    ASSERT(ptr_, "Invalid access of null pointer.");
    return ptr_;
  }

  // De-reference operators.
  decltype(auto) operator->() const { return get(); }
  decltype(auto) operator*() const { return *get(); }

  // Check if owning ptr still lives.
  constexpr operator bool() const { return ptr_ != nullptr; }

  // Access w/o any checking.
  constexpr T* get_unchecked() const { return ptr_; }

 private:
  T* ptr_;
};

// Test for equality.
template <typename T>
constexpr bool operator==(const NonNullPtr<T>& a, const NonNullPtr<T>& b) {
  return a.get_unchecked() == b.get_unchecked();
}
template <typename T>
constexpr bool operator==(const NonNullPtr<T>& a, const T* b) {
  return a.get_unchecked() == b;
}
template <typename T>
constexpr bool operator==(const T* a, const NonNullPtr<T>& b) {
  return a == b.get_unchecked();
}

// Test for inequality.
template <typename T>
constexpr bool operator!=(const NonNullPtr<T>& a, const NonNullPtr<T>& b) {
  return a.get_unchecked() != b.get_unchecked();
}
template <typename T>
constexpr bool operator!=(const NonNullPtr<T>& a, const T* b) {
  return a.get_unchecked() != b;
}
template <typename T>
constexpr bool operator!=(const T* a, const NonNullPtr<T>& b) {
  return a != b.get_unchecked();
}

}  // namespace math

// Specialization of std::hash
namespace std {
template <class T>
struct hash<math::NonNullPtr<T>> {
  std::size_t operator()(const math::NonNullPtr<T>& ptr) const {
    return std::hash<const T*>{}(ptr.get_unchecked());
  }
};
}  // namespace std
