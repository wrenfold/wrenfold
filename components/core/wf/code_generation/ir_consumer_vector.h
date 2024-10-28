// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/code_generation/ir_value_fwd.h"
#include "wf/utility/third_party_imports.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf::ir {

// Stores a sparse vector of value_ptrs. Empty cells are denoted by a nullptr. When an insertion
// occurs, we consult the free list to find a place to insert the new element. When a removal
// occurs, we nullify the corresponding cell and add its index to the free list.
// TODO: Would like to clean this up and generalize it a bit into a general purpose container.
class consumer_vector {
 public:
  using container_type = absl::InlinedVector<maybe_null<ir::value*>, 8>;

  // Skip iterator that passes over null values in our storage array.
  class const_iterator {
   public:
    using value_type = ir::value_ptr;
    using difference_type = std::ptrdiff_t;
    using pointer = void;
    using reference = void;
    using iterator_category = std::input_iterator_tag;

    constexpr bool operator==(const const_iterator& other) const noexcept {
      return it_ == other.it_;
    }
    constexpr bool operator!=(const const_iterator& other) const noexcept {
      return !operator==(other);
    }

    constexpr const_iterator& operator++() noexcept {
      ++it_;
      advance_to_next();
      return *this;
    }

    constexpr const_iterator operator++(int) noexcept {
      const const_iterator copy = *this;
      ++it_;
      advance_to_next();
      return copy;
    }

    // Occupied cells are never null, so convert to value_ptr.
    constexpr value_type operator*() const noexcept { return ir::value_ptr{it_->get_unchecked()}; }

   private:
    explicit constexpr const_iterator(const container_type::const_iterator it,
                                      const container_type::const_iterator end_it) noexcept
        : it_(it), end_it_(end_it) {
      advance_to_next();
    }

    constexpr void advance_to_next() noexcept {
      for (; it_ != end_it_ && !it_->has_value(); ++it_) {
      }
    }

    friend class consumer_vector;

    container_type::const_iterator it_;
    container_type::const_iterator end_it_;
  };

  // Insert the provided value. Returns the index indicating where this value was stored.
  std::size_t insert(ir::value* v) {
    if (freelist_.empty()) {
      const std::size_t index = consumers_.size();
      consumers_.emplace_back(v);
      return index;
    } else {
      const std::size_t index = freelist_.back();
      freelist_.pop_back();
      consumers_[index] = v;
      return index;
    }
  }

  // Remove value indicated by the specified index.
  void remove(const std::size_t index) {
    WF_ASSERT_LT(index, consumers_.size());
    WF_ASSERT(consumers_[index]);
    consumers_[index] = nullptr;
    freelist_.push_back(static_cast<std::uint32_t>(index));
  }

  // True if there are no consumers.
  bool empty() const noexcept { return size() == 0; }

  // Number of consumers.
  std::size_t size() const noexcept { return consumers_.size() - freelist_.size(); }

  auto begin() const noexcept { return const_iterator{consumers_.begin(), consumers_.end()}; }
  auto end() const noexcept { return const_iterator{consumers_.end(), consumers_.end()}; }

  void clear() noexcept {
    consumers_.clear();
    freelist_.clear();
  }

 private:
  absl::InlinedVector<maybe_null<ir::value*>, 8> consumers_;
  absl::InlinedVector<std::uint32_t, 8> freelist_;
};

}  // namespace wf::ir
