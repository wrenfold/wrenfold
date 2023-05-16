// Copyright 2023 Gareth Cross
#pragma once
#include <memory>
#include <type_traits>

#include "assertions.h"

namespace math {

template <typename T>
class OwningPtr;

namespace detail {

// Non-thread-safe reference counter.
class RefCounter {
 public:
  // Move construct.
  RefCounter(RefCounter&& other) noexcept : data_(other.data_) { other.data_ = nullptr; }

  // Move assign.
  RefCounter& operator=(RefCounter&& other) noexcept {
    if (this != &other) {
      data_ = other.data_;
      other.data_ = nullptr;
    }
    return *this;
  }

  // Copy construct:
  RefCounter(const RefCounter& other) noexcept : data_(other.data_) {
    if (data_) {
      // increment the weak reference count
      data_->ref_count++;
    }
  }

  // Copy assign:
  RefCounter& operator=(const RefCounter& other) noexcept {
    if (this != &other) {
      DecrementCount();
      data_ = other.data_;
      if (data_) {
        data_->ref_count++;
      }
    }
    return *this;
  }

  // Destroy.
  ~RefCounter() noexcept { DecrementCount(); }

  // True if we can still access the original pointer.
  bool IsValid() const { return data_ && data_->ptr_valid; }

  // Access the underlying reference count.
  uint32_t GetCount() const { return data_ ? data_->ref_count : 0; }

 private:
  struct Data {
    // Reference count. One for the original OwningPtr, plus all other weak ptrs.
    uint32_t ref_count;
    // Is the original owned ptr still valid?
    bool ptr_valid;
  };

  Data* data_;

  // Private, only OwningPtr can call this.
  RefCounter() : data_(new Data()) {
    data_->ref_count = 1;
    data_->ptr_valid = true;
  }

  // Decrement the reference count. If the count is zero, destroy the data object.
  void DecrementCount() noexcept {
    if (data_) {
      data_->ref_count--;
      // When count reaches zero, we can destroy this.
      if (data_->ref_count == 0) {
        delete data_;
        data_ = nullptr;
      }
    }
  }

  // Only OwningPtr can manipulate `ptr_valid`.
  template <typename T>
  friend class math::OwningPtr;
};

static_assert(std::is_nothrow_move_constructible_v<RefCounter>);
static_assert(std::is_nothrow_move_assignable_v<RefCounter>);

}  // namespace detail

// A non-owning pointer. May be constructed from OwningPtr. Keeps tracking of whether the original
// OwningPtr is still valid via a simple (non thread safe) reference counting mechanism.
template <typename T>
class NonOwningPtr {
 public:
  // Access the underlying pointer, w/ a check for validity first.
  T* get() const {
    ASSERT(counter_.IsValid(), "Invalid access of non-owning ptr.");
    return ptr_;
  }

  // De-reference operators.
  decltype(auto) operator->() const { return get(); }
  decltype(auto) operator*() const { return *get(); }

  // Check if owning ptr still lives.
  constexpr operator bool() const { return counter_.IsValid(); }

  // Access w/o any checking.
  constexpr T* get_unchecked() const { return ptr_; }

  // For unit tests: Access the reference count.
  uint32_t GetCount() const { return counter_.GetCount(); }

 private:
  friend class OwningPtr<T>;

  T* ptr_{nullptr};
  detail::RefCounter counter_;

  // Private constructor for use by OwningPtr
  explicit NonOwningPtr(T* ptr, detail::RefCounter counter) noexcept
      : ptr_(ptr), counter_(std::move(counter)) {}
};

struct DummyType {};
static_assert(std::is_nothrow_move_constructible_v<NonOwningPtr<DummyType>>);
static_assert(std::is_nothrow_move_assignable_v<NonOwningPtr<DummyType>>);

// non-copyable, and non-nullable...
template <typename T>
class OwningPtr {
 public:
  // Construct from bare pointer.
  explicit OwningPtr(T* ptr) : OwningPtr(std::make_unique<T>(ptr)) {}

  // Construct from unique_ptr.
  explicit OwningPtr(std::unique_ptr<T>&& ptr) : ptr_(std::move(ptr)), ref_counter_() {
    ASSERT(ptr_, "Cannot construct null OwningPtr");
  }

  // Cannot construct from nullptr.
  [[maybe_unused]] OwningPtr(std::nullptr_t) = delete;

  // Move-construct.
  OwningPtr(OwningPtr&& other) noexcept
      : ptr_(std::move(other.ptr_)), ref_counter_(std::move(other.ref_counter_)) {}

  //  Move-assign.
  OwningPtr& operator=(OwningPtr&& other) noexcept {
    if (this != &other) {
      Invalidate();
      ptr_ = std::move(other.ptr_);
      ref_counter_ = std::move(other.ref_counter_);
    }
    return *this;
  }

  // On destruction, we mark the RefCounter invalid. This prohibits any existing NonOwningPtr from
  // accessing the underlying data.
  ~OwningPtr() noexcept { Invalidate(); }

  // Implicit cast to non-owning pointer.
  [[nodiscard]] operator NonOwningPtr<T>() const {
    // Catch making non-owning ptr using a "moved-from" owning ptr:
    ASSERT(ptr_, "Cannot make a non owning pointer from empty owning ptr");
    return NonOwningPtr<T>(ptr_.get(), ref_counter_);
  }

  // Access underlying pointer.
  T* get() const { return ptr_.get(); }

  decltype(auto) operator->() const { return get(); }
  decltype(auto) operator*() const { return *get(); }

  // Check if `ptr_` is valid, which should be true unless we moved from this object.
  constexpr operator bool() const { return static_cast<bool>(ptr_); }

  // For unit tests: Access the reference count.
  uint32_t GetCount() const { return ref_counter_.GetCount(); }

 private:
  void Invalidate() noexcept {
    if (ref_counter_.data_) {
      // We set ptr_valid false. The destructor of `RefCounter` takes care of the rest.
      ref_counter_.data_->ptr_valid = false;
    }
  }

  std::unique_ptr<T> ptr_;
  detail::RefCounter ref_counter_;
};

static_assert(std::is_nothrow_move_constructible_v<OwningPtr<DummyType>>);
static_assert(std::is_nothrow_move_assignable_v<OwningPtr<DummyType>>);

// Make an owning ptr by invoking the constructor of `T` w/ `args`.
template <typename T, typename... Args>
OwningPtr<T> MakeOwningPtr(Args&&... args) {
  return OwningPtr(std::make_unique<T>(std::forward<Args>(args)...));
}

// Test for equality.
template <typename T>
constexpr bool operator==(const NonOwningPtr<T>& a, const NonOwningPtr<T>& b) {
  return a.get_unchecked() == b.get_unchecked();
}

template <typename T>
constexpr bool operator==(const OwningPtr<T>& a, const NonOwningPtr<T>& b) {
  return a.get() == b.get_unchecked();
}

template <typename T>
constexpr bool operator==(const NonOwningPtr<T>& a, const OwningPtr<T>& b) {
  return a.get_unchecked() == b.get();
}

}  // namespace math
