// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once

namespace wf {

// A simple range type to allow forward-iterating over a sequence of integers as though
// they were a container. Only intended to contain primitive integers.
template <typename T>
struct index_range {
  struct iterator {
   public:
    constexpr bool operator==(const iterator& other) const noexcept {
      return value_ == other.value_ && stop_ == other.stop_;
    }

    constexpr bool operator!=(const iterator& other) const noexcept {
      return value_ != other.value_ || stop_ != other.stop_;
    }

    constexpr iterator& operator++() noexcept {
      ++value_;
      return *this;
    }

    constexpr iterator operator++(int) noexcept {
      iterator copy = *this;
      ++value_;
      return copy;
    }

    constexpr const T& operator*() const noexcept { return value_; }

    constexpr iterator(T value, T stop) noexcept : value_(value), stop_(stop) {}

   private:
    T value_;
    T stop_;
  };

  constexpr index_range(T start, T stop) : start_(start), stop_(stop) {}

  constexpr auto begin() const noexcept { return iterator{start_, stop_}; }
  constexpr auto end() const noexcept { return iterator{stop_, stop_}; }
  constexpr auto cbegin() const noexcept { return begin(); }
  constexpr auto cend() const noexcept { return end(); }

 private:
  T start_;
  T stop_;
};

template <typename T>
constexpr auto make_range(T start, T stop) noexcept {
  return index_range<T>{start, stop};
}

template <typename T>
constexpr auto make_range(T stop) noexcept {
  return index_range<T>{static_cast<T>(0), stop};
}

static_assert(*(++make_range(0, 5).begin()) == 1);
static_assert(*(++(++make_range(0, 5).begin())) == 2);
static_assert(*((++make_range(2, 5).begin())++) == 3);

}  // namespace wf
