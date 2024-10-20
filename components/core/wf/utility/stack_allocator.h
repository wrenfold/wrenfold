// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <memory>
#include <type_traits>

// A simple custom stack allocator.
//
// While implementing this I referred to:
// Andrei Alexandrescu's talk: https://www.youtube.com/watch?v=LIb3L4vKZ7U
// https://blog.andreiavram.ro/cpp-custom-allocators-first-step
// https://github.dev/FelixPetriconi/AllocatorBuilder
//
// For now I don't implement the composable adapters from Andrei's talk, since stack +
// std::allocator is the only pair I need initially.
namespace wf {

namespace internal {
// Round up `n` to the nearest `alignment` bytes.
constexpr size_t round_to_alignment(const std::size_t alignment, std::size_t n) noexcept {
  const std::size_t remainder = n % alignment;
  if (remainder != 0) {
    n += alignment - remainder;
  }
  return n;
}
}  // namespace internal

// Store a stack-allocated buffer with the given size + alignment.
// We have a pointer `p_` that indicates the first space in the stack. When allocations are
// performed, we check if capacity exists and then move p_ forward. All allocations and
// deallocations must be FILO ordered.
template <std::size_t Capacity, std::size_t Alignment>
class stack_allocator {
 public:
  using size_type = std::size_t;
  static constexpr std::size_t capacity = Capacity;

  constexpr stack_allocator() noexcept : p_(&data_[0]) {}

  // Allocate storage for `n` bytes.
  [[nodiscard]] constexpr void* allocate(const size_type n) noexcept {
    if (n == 0) {
      return nullptr;
    }
    const std::size_t n_aligned = internal::round_to_alignment(Alignment, n);
    if (p_ + n_aligned > data_ + Capacity) {
      // Insufficient space.
      return nullptr;
    }
    const auto ptr = p_;
    p_ += n_aligned;
    return static_cast<void*>(ptr);
  }

  // Deallocate storage for `n` bytes, but only if `[ptr, n]` was the last block we allocated.
  // It is assumed that `owns` has already been checked before invoking this.
  constexpr void deallocate(void* ptr, const size_type n) noexcept {
    const std::size_t n_aligned = internal::round_to_alignment(Alignment, n);
    if (static_cast<char*>(ptr) + n_aligned == p_) {
      p_ = static_cast<char*>(ptr);
    }
  }

  // Test if the allocator owns the provided ptr.
  constexpr bool owns(const void* ptr) const noexcept {
    return ptr >= data_ && ptr < data_ + Capacity;
  }

  // True if the provided pointer is owned by this allocator, and is in the used region of the
  // stack. This is for unit testing.
  constexpr bool in_live_range(const void* ptr) const noexcept { return ptr >= data_ && ptr < p_; }

  // True if the allocator is completely empty.
  constexpr bool is_empty() const noexcept { return p_ == &data_[0]; }

  // Number of bytes that are allocated.
  constexpr std::size_t used_bytes() const noexcept {
    return static_cast<std::size_t>(reinterpret_cast<std::ptrdiff_t>(p_) -
                                    reinterpret_cast<std::ptrdiff_t>(&data_[0]));
  }

  // Only equivalent to self.
  constexpr bool operator==(const stack_allocator& other) const noexcept { return this == &other; }
  constexpr bool operator!=(const stack_allocator& other) const noexcept {
    return !operator==(other);
  }

  // Not copyable or movable since ownership of the stack cannot be meaningfully transferred.
  stack_allocator(const stack_allocator&) = delete;
  stack_allocator(stack_allocator&&) = delete;
  stack_allocator& operator=(const stack_allocator&) = delete;
  stack_allocator& operator=(stack_allocator&&) = delete;

 private:
  alignas(Alignment) char data_[Capacity];
  char* p_;
};

// Pair together a stack allocator with a fallback allocator, and implement the STL interface for
// allocators so we can use this with containers:
// https://en.cppreference.com/w/cpp/named_req/Allocator
template <typename T, std::size_t Capacity>
class stl_stack_allocator_with_fallback {
 public:
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using value_type = T;
  using stack_allocator_type = stack_allocator<Capacity, alignof(T)>;

  // Construct with non-own reference to a stack allocator.
  explicit stl_stack_allocator_with_fallback(stack_allocator_type& s) noexcept : s_(s) {}

  template <typename U>
  struct rebind {
    using other = stl_stack_allocator_with_fallback<U, Capacity>;
  };

  stl_stack_allocator_with_fallback() noexcept = default;

  // Constructor to allow rebinding to a type `U` where U is not T.
  template <typename U, typename = std::enable_if_t<alignof(T) == alignof(U)>>
  explicit stl_stack_allocator_with_fallback(
      const stl_stack_allocator_with_fallback<U, Capacity>& other) noexcept
      : s_(other.primary()), f_() {}

  // Allocate space for `n` objects. First tries to allocate on the stack. If no capacity is left,
  // we fall back to using the std::allocator implementation.
  [[nodiscard]] T* allocate(const std::size_t n) {
    if (auto ptr = static_cast<T*>(s_.allocate(sizeof(T) * n)); ptr != nullptr) {
      return ptr;
    }
    return f_.allocate(n);
  }

  // Deallocate space for `n` objects. First check if the objects were on the stack. If they were
  // not, we fall back to the std::allocator implementation.
  void deallocate(T* ptr, const std::size_t n) {
    if (s_.owns(static_cast<void*>(ptr))) {
      s_.deallocate(static_cast<void*>(ptr), sizeof(T) * n);
    } else {
      f_.deallocate(ptr, n);
    }
  }

  bool operator==(const stl_stack_allocator_with_fallback& other) const noexcept {
    return s_ == other.s_ && f_ == other.f_;
  }
  bool operator!=(const stl_stack_allocator_with_fallback& other) const noexcept {
    return s_ != other.s_ || f_ != other.f_;
  }

  constexpr stack_allocator_type& primary() const noexcept { return s_; }

 private:
  stack_allocator_type& s_;
  std::allocator<T>
      f_;  // TODO: Use private inheritance so we get the zero-size base class optimization.
};

}  // namespace wf
