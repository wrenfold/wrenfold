// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <bitset>

#include "wf/utility/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <fmt/core.h>
#include <fmt/ranges.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

// Iterate over positions of active bits in a bitset.
// This is mostly so we can print the active bit positions using fmt::join(...).
template <std::size_t N>
class bitset_range {
 public:
  using bitset_type = std::bitset<N>;

  class const_iterator {
   public:
    using difference_type = std::ptrdiff_t;
    using value_type = std::size_t;
    using pointer = void;
    using reference = void;
    using iterator_category = std::input_iterator_tag;

    bool operator==(const const_iterator& other) const noexcept {
      return pos_ == other.pos_ && &b_ == &other.b_;
    }

    bool operator!=(const const_iterator& other) const noexcept { return !operator==(other); }

    const_iterator& operator++() noexcept {
      ++pos_;
      advance();
      return *this;
    }

    const_iterator operator++(int) noexcept {
      const const_iterator copy = *this;
      ++pos_;
      return copy;
    }

    constexpr std::size_t operator*() const noexcept { return pos_; }

    // Construct with reference to bitset and starting position.
    const_iterator(const bitset_type& b, const std::size_t pos) noexcept : b_(b), pos_(pos) {
      advance();
    }

   private:
    // Advance to the next active bit.
    void advance() {
      for (; pos_ < b_.size() && !b_[pos_]; ++pos_) {
      }
    }
    const bitset_type& b_;
    std::size_t pos_;
  };

  constexpr explicit bitset_range(const bitset_type& b) noexcept : b_(b) {}

  auto begin() const noexcept { return const_iterator{b_, 0}; }
  auto end() const noexcept { return const_iterator{b_, b_.size()}; }
  auto cbegin() const noexcept { return begin(); }
  auto cend() const noexcept { return end(); }

 private:
  const bitset_type& b_;
};

// A user-defined deduction guide for `bitset_range`.
template <std::size_t N>
bitset_range(std::bitset<N>) -> bitset_range<N>;

}  // namespace wf
