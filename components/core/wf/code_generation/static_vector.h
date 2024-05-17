// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <array>

#include "wf/assertions.h"

namespace wf {

// A simple static_vector type. Note:
// - This type uses std::array and assumes objects are default constructible.
// - I expect to use this to store trivially copyable things, no attention has been paid to move
//   operators, etc.
template <typename T, std::size_t N>
class static_vector {
 public:
  using value_type = T;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using iterator = pointer;
  using const_iterator = const_pointer;

  // Default construct all elements.
  constexpr static_vector() noexcept = default;

  // Copy-construct from initializer list.
  static_vector(std::initializer_list<T> init) : size_(init.end() - init.begin()) {
    WF_ASSERT_LESS_OR_EQ(size_, N);
    std::copy(init.begin(), init.end(), begin());
  }

  constexpr size_type size() const noexcept { return size_; }

  // True if the are no elements.
  constexpr bool empty() const noexcept { return size_ == 0; }

  // Max capacity of the static vector.
  constexpr std::size_t max_size() const noexcept { return N; }

  // True if all capacity is consumed.
  constexpr bool full() const noexcept { return size_ == max_size(); }

  constexpr const_iterator begin() const noexcept { return &data_[0]; }
  constexpr const_iterator end() const noexcept { return begin() + size_; }

  constexpr const_iterator cbegin() const noexcept { return begin(); }
  constexpr const_iterator cend() const noexcept { return end(); }

  constexpr iterator begin() noexcept { return &data_[0]; }
  constexpr iterator end() noexcept { return begin() + size_; }

  constexpr const_reference operator[](const std::size_t pos) const {
#ifdef WF_DEBUG
    WF_ASSERT_LESS(pos, size());
#endif
    return data_[pos];
  }

  constexpr reference operator[](const std::size_t pos) {
#ifdef WF_DEBUG
    WF_ASSERT_LESS(pos, size());
#endif
    return data_[pos];
  }

  // Insert at the back of the array.
  constexpr void push_back(const value_type& value) {
#ifdef WF_DEBUG
    WF_ASSERT(!full());
#endif
    data_[size_] = value;
    ++size_;
  }

  // Insert at `pos`. Returns iterator to inserted element.
  iterator insert(const_iterator pos, const value_type& value) {
#ifdef WF_DEBUG
    WF_ASSERT(!full());
#endif
    iterator mutable_position = const_cast<iterator>(pos);
    std::move_backward(mutable_position, end(), end() + 1);
    *mutable_position = value;
    ++size_;
    return mutable_position;
  }

  // Check for equality w/ another static vector of same type.
  template <std::size_t M>
  bool operator==(const static_vector<T, M>& other) const {
    return size_ == other.size() && std::equal(begin(), end(), other.begin());
  }

 private:
  std::array<T, N> data_;
  size_type size_{0};
};

}  // namespace wf
