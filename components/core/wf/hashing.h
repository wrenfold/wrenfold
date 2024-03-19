// Copyright 2023 Gareth Cross
#pragma once
#include <array>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace wf {

// Hash object T.
// Specializations should implement `std::size_t operator()(const T&) const`
template <typename T, typename = void>
struct hash_struct;

// constexpr FNV hash of string_view
constexpr std::size_t hash_string_fnv(const std::string_view& str) noexcept {
  constexpr std::size_t fnv_offset = 0xcbf29ce484222325;
  constexpr std::size_t fnv_prime = 0x100000001b3;
  std::size_t result = fnv_offset;
  for (const char c : str) {
    result = (result * fnv_prime) ^ static_cast<std::size_t>(c);
  }
  return result;
}

// Based on https://stackoverflow.com/questions/2590677/
// TODO: Investigate if there is something better than this.
// The special numeric value here is 2^64 divided by the golden ratio, as uint64_t.
constexpr std::size_t hash_combine(const std::size_t seed, const std::size_t new_hash) noexcept {
  static_assert(sizeof(std::size_t) == 8);
  return seed ^ (new_hash + 0x9e3779b97f4a7c15 + (seed << 6) + (seed >> 2));
}

// Hash a container of objects by traversing a range specified by iterators.
template <typename Iterator>
std::size_t hash_all(std::size_t seed, Iterator begin, Iterator end) {
  using T = std::decay_t<decltype(*begin)>;
  for (; begin != end; ++begin) {
    seed = hash_combine(seed, hash_struct<T>{}(*begin));
  }
  return seed;
}

// Hash a container of objects.
template <typename Container>
std::size_t hash_all(std::size_t seed, const Container& container) {
  return hash_all(seed, std::begin(container), std::end(container));
}

// Hash a variadic list of arguments.
template <typename... Ts>
std::size_t hash_args(std::size_t seed, const Ts&... expressions) {
  const std::array<std::size_t, sizeof...(Ts)> hashes = {hash_struct<Ts>{}(expressions)...};
  for (std::size_t i = 0; i < hashes.size(); ++i) {
    seed = hash_combine(seed, hashes[i]);
  }
  return seed;
}

// Shorthand for invoking `hash_struct<T>` on an object.
template <typename T>
std::size_t hash(const T& object) noexcept(std::is_nothrow_invocable_v<hash_struct<T>, const T&>) {
  return hash_struct<T>{}(object);
}

// Implement hash_struct for strings.
template <>
struct hash_struct<std::string> {
  std::size_t operator()(const std::string& str) const noexcept {
    return hash_string_fnv(std::string_view{str});
  }
};

// Inherit to implement hashing of a variant.
// Combines the `index` the hash of the underlying contents.
template <typename T>
struct hash_variant;
template <typename... Ts>
struct hash_variant<std::variant<Ts...>> {
  // Will throw std::bad_variant_access (and possibly terminate) if `v` is valueless_by_exception.
  std::size_t operator()(const std::variant<Ts...>& v) const
      noexcept(std::conjunction_v<std::is_nothrow_invocable<hash_struct<Ts>, const Ts&>...>) {
    return hash_combine(v.index(),
                        std::visit([](const auto& x) -> std::size_t { return wf::hash(x); }, v));
  }
};

// Check if two objects are identical.
// Specializations should implement `bool operator()(const T&, const T&) const`.
template <typename T, typename = void>
struct is_identical_struct;

// True if all the elements of containers `a` and `b` are identical.
template <typename Container>
bool all_identical(const Container& a, const Container& b) {
  if (a.size() != b.size()) {
    return false;
  }
  // Determine the underlying type in the container:
  using T = std::decay_t<decltype(*std::begin(a))>;
  return std::equal(std::begin(a), std::end(a), std::begin(b), is_identical_struct<T>{});
}

// Implement `is_identical_struct` for integrals + enums.
template <typename T>
struct is_identical_struct<T, std::enable_if_t<std::is_integral_v<T> || std::is_enum_v<T>>> {
  constexpr bool operator()(T a, T b) const noexcept { return a == b; }
};

// Implement `is_identical_struct` for vectors.
template <typename T>
struct is_identical_struct<std::vector<T>> {
  bool operator()(const std::vector<T>& a, const std::vector<T>& b) const
      noexcept(std::is_nothrow_invocable_v<is_identical_struct<T>, const T&, const T&>) {
    if (a.size() != b.size()) {
      return false;
    }
    return std::equal(a.begin(), a.end(), b.begin(), is_identical_struct<T>{});
  }
};

// Inherit to implement `is_identical_struct` for a variant.
// Variants must have the same index to be identical.
template <typename T>
struct is_identical_variant;
template <typename... Ts>
struct is_identical_variant<std::variant<Ts...>> {
  std::size_t operator()(const std::variant<Ts...>& a, const std::variant<Ts...>& b) const
      noexcept(std::conjunction_v<
               std::is_nothrow_invocable<is_identical_struct<Ts>, const Ts&, const Ts&>...>) {
    if (a.index() != b.index()) {
      return false;
    }
    return std::visit(
        [&](const auto& a_typed) -> bool {
          // Types are known to match at this juncture:
          using T = std::decay_t<decltype(a_typed)>;
          return is_identical_struct<T>{}(a_typed, std::get<T>(b));
        },
        a);
  }
};

// True if two instances of type `T` are identical.
template <typename T>
bool are_identical(const T& a, const T& b) noexcept(
    std::is_nothrow_invocable_v<is_identical_struct<T>, const T&, const T&>) {
  return is_identical_struct<T>{}(a, b);
}

}  // namespace wf
