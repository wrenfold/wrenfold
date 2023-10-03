// Copyright 2023 Gareth Cross
#pragma once

namespace math {

// A simple range type to allow forward-iterating over a sequence of integers as though
// they were a container. Only intended to contain primitive integers.
template <typename T>
struct IndexRange {
  struct Iterator {
   public:
    constexpr bool operator==(const Iterator& other) const {
      return value_ == other.value_ && stop_ == other.stop_;
    }

    constexpr bool operator!=(const Iterator& other) const {
      return value_ != other.value_ || stop_ != other.stop_;
    }

    constexpr Iterator& operator++() {
      ++value_;
      return *this;
    }

    constexpr Iterator operator++(int) {
      Iterator copy = *this;
      ++value_;
      return copy;
    }

    constexpr const T& operator*() const { return value_; }

    constexpr Iterator(T value, T stop) : value_(value), stop_(stop) {}

   private:
    T value_;
    T stop_;
  };

  constexpr IndexRange(T start, T stop) : start_(start), stop_(stop) {}

  constexpr auto begin() const { return Iterator{start_, stop_}; }
  constexpr auto end() const { return Iterator{stop_, stop_}; }
  constexpr auto cbegin() const { return begin(); }
  constexpr auto cend() const { return end(); }

 private:
  T start_;
  T stop_;
};

template <typename T>
constexpr auto make_range(T start, T stop) {
  return IndexRange<T>{start, stop};
}

static_assert(*(++make_range(0, 5).begin()) == 1);
static_assert(*(++(++make_range(0, 5).begin())) == 2);
static_assert(*((++make_range(2, 5).begin())++) == 3);

}  // namespace math
