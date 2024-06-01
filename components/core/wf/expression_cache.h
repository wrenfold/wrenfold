// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <tuple>
#include <unordered_map>

#include "wf/any_expression.h"
#include "wf/utility/type_list.h"

namespace wf {

struct reserve_hint {
  constexpr explicit reserve_hint(const std::size_t v) noexcept : value(v) {}
  std::size_t value;
};

template <typename T>
struct reserve_type_hint {
  constexpr explicit reserve_type_hint(const std::size_t v) noexcept : value(v) {}
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

  // Construct and reserve, for one type only.
  template <typename T>
  explicit expression_map_tuple(const reserve_type_hint<T> hint) {
    get<T>().reserve(hint.value);
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

// Store a tuple of maps: X -> X, where `X` is an expression type like `scalar_expr`.
class expression_cache {
 public:
  expression_cache() noexcept = default;

  // Construct and reserve the maps using `hint` to determine the size.
  explicit expression_cache(const reserve_hint hint) : storage_(hint) {}

  // Construct and reserve, for one type only.
  template <typename T>
  explicit expression_cache(const reserve_type_hint<T> hint) : storage_(hint) {}

  // Look-up `expression` in the cache. Return it if it exists, otherwise compute and insert a
  // value by calling lambda `f`.
  template <typename T, typename F>
  const auto& get_or_insert(const T& expression, F&& f) {
    // Figure out which map type `T` is stored in:
    auto& map = storage_.get<T>();
    if (auto it = map.find(expression); it != map.end()) {
      return it->second;
    }
    const auto [insertion_it, _] = map.emplace(expression, f(expression));
    return insertion_it->second;
  }

 private:
  // TODO: Profile absl_flat_map here.
  template <typename K>
  using map_type = std::unordered_map<K, K, hash_struct<K>, is_identical_struct<K>>;

  expression_map_tuple<map_type> storage_;
};

}  // namespace wf
