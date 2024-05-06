// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <tuple>
#include <type_traits>
#include <unordered_map>

#include "wf/any_expression.h"
#include "wf/type_list.h"

namespace wf {

// When specified as the `OutputType` of the cache, the value type of each map in the cache will
// match its key. This is so we can map `scalar_expr --> scalar_expr`, `boolean_expr -->
// boolean_expr`, etc.
struct output_is_input_t {};

namespace detail {

template <typename Key, typename Value>
struct cache_map_type {
  using type =
      std::unordered_map<Key,
                         std::conditional_t<std::is_same_v<Value, output_is_input_t>, Key, Value>,
                         hash_struct<Key>, is_identical_struct<Key>>;
};

template <typename Key, typename Value>
using cache_map_type_t = typename cache_map_type<Key, Value>::type;

}  // namespace detail

struct reserve_hint {
  constexpr explicit reserve_hint(std::size_t v) noexcept : value(v) {}
  std::size_t value;
};

// Store a tuple of maps - one per type of expression that we support. Maps can be retrieved by
// their key type.
template <template <typename> class Map>
class expression_map_tuple {
 public:
  // All the different expressiont types.
  using expression_types_list = type_list_from_variant_t<any_expression>;

  // The type we will store, a tuple of maps.
  using tuple_type = tuple_from_type_list_t<type_list_map_t<Map, expression_types_list>>;

  expression_map_tuple() noexcept = default;

  // Construct and reserve space in maps.
  explicit expression_map_tuple(const reserve_hint hint) {
    std::apply([hint](auto&... maps) { (maps.reserve(hint.value), ...); }, storage_);
  }

  // Get the map for the specified type.
  template <typename T, typename = enable_if_contains_type_t<T, expression_types_list>>
  constexpr auto& get() noexcept {
    return std::get<type_list_index_v<T, expression_types_list>>(storage_);
  }

  // Get the map for the specified type, const version.
  template <typename T, typename = enable_if_contains_type_t<T, expression_types_list>>
  constexpr const auto& get() const noexcept {
    return std::get<type_list_index_v<T, expression_types_list>>(storage_);
  }

  // Get the underlying tuple.
  constexpr const tuple_type& storage() const noexcept { return storage_; }

 private:
  tuple_type storage_;
};

// Store a tuple of maps. There is one element in the tuple for every expression type.
template <typename OutputType = output_is_input_t>
class expression_cache {
 private:
  // Determine the value types we keep in the cache.
  using expression_type_list = type_list<scalar_expr, boolean_expr, matrix_expr, compound_expr>;

  template <typename Key>
  using map_type_t = detail::cache_map_type_t<Key, OutputType>;
  using tuple_type = tuple_from_type_list_t<type_list_map_t<map_type_t, expression_type_list>>;

 public:
  expression_cache() { std::get<0>(storage_).reserve(50); }

  // Look-up `expression` in the cache. Return it if it exists, otherwise compute and insert a value
  // by calling lambda `f`.
  template <typename T, typename F>
  const auto& get_or_insert(const T& expression, F&& f) {
    // Figure out which map type `T` is stored in:
    auto& map = std::get<type_list_index_v<T, expression_type_list>>(storage_);
    if (auto it = map.find(expression); it != map.end()) {
      return it->second;
    }
    const auto [insertion_it, _] = map.emplace(expression, f(expression));
    return insertion_it->second;
  }

  // Find an element if an exists in the cache, otherwise return nullopt.
  template <typename T>
  auto find(const T& expression) const {
    auto& map = std::get<type_list_index_v<T, expression_type_list>>(storage_);
    using value_type = typename std::remove_reference_t<decltype(map)>::mapped_type;
    if (auto it = map.find(expression); it != map.end()) {
      // TODO: Add a version that does not copy into an optional?
      return std::optional<value_type>{it->second};
    }
    return std::optional<value_type>{std::nullopt};
  }

 private:
  tuple_type storage_;
};

}  // namespace wf
