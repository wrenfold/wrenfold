// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/types.h"

#include <algorithm>
#include <iterator>

#include "wf/utility/algorithms.h"
#include "wf/utility/assertions.h"
#include "wf/utility/ordering.h"
#include "wf/utility/overloaded_visit.h"

WF_BEGIN_THIRD_PARTY_INCLUDES
#include <absl/container/inlined_vector.h>
WF_END_THIRD_PARTY_INCLUDES

namespace wf {

struct_field::struct_field(std::string name, type_variant type)
    : struct_field(std::move(name), std::move(type), {}) {}

struct_field::struct_field(std::string name, type_variant type, native_field_accessor accessor)
    : name_(std::move(name)), type_(std::move(type)), native_accessor_(std::move(accessor)) {
  WF_ASSERT(!name_.empty(), "Field names may not be empty strings");
}

// Check that all provided field names are unique.
static void assert_field_names_are_unique(const std::vector<struct_field>& fields) {
  if (fields.empty()) {
    return;
  }
  auto names = transform_map<absl::InlinedVector<std::string_view, 8>>(
      fields, [](const struct_field& f) -> std::string_view { return f.name(); });
  std::sort(names.begin(), names.end());

  for (auto it = names.begin(); std::next(it) != names.end(); ++it) {
    WF_ASSERT(*it != *std::next(it), "Custom type has duplicated field name: {}", *it);
  }
}

// We don't consider the hash of `underlying_type` here. This is because python hashes and
// type_index hashes are platform specific and can change at runtime. This will inject additional
// non-determinism, which I would prefer to avoid. The odds of hash collisions are somewhat low,
// since the two types would need identical names and member names.
std::shared_ptr<const custom_type::impl> custom_type::create_impl(
    std::string name, std::vector<struct_field> fields, underlying_type_variant underlying_type) {
  impl result{std::move(name), std::move(fields), std::move(underlying_type), 0};
  result.hash = hash_string_fnv(result.name);
  result.hash = hash_all(result.hash, result.fields);
  return std::make_shared<impl>(std::move(result));
}

custom_type::custom_type(std::string name, std::vector<struct_field> fields,
                         underlying_type_variant underlying_type)
    : impl_{create_impl(std::move(name), std::move(fields), std::move(underlying_type))} {
  assert_field_names_are_unique(impl_->fields);
}

maybe_null<const struct_field*> custom_type::field_by_name(std::string_view name) const noexcept {
  // fields_ will typically be pretty small, so just do a linear search:
  const auto it = std::find_if(impl_->fields.begin(), impl_->fields.end(),
                               [&name](const struct_field& f) { return f.name() == name; });
  if (it == impl_->fields.end()) {
    return nullptr;
  }
  return &*it;
}

struct count_custom_type_size {
  constexpr std::size_t operator()(const scalar_type&) const noexcept { return 1; }
  constexpr std::size_t operator()(const matrix_type& m) const noexcept { return m.size(); }

  std::size_t operator()(const custom_type& c) const noexcept {
    // Append every field on this type, and recurse as well into child custom types.
    std::size_t total = 0;
    for (const struct_field& field : c.fields()) {
      total += std::visit(*this, field.type());
    }
    return total;
  }
};

std::size_t custom_type::size() const noexcept { return impl_->fields.size(); }

std::size_t custom_type::total_size() const noexcept { return count_custom_type_size{}(*this); }

bool is_identical_struct<custom_type>::operator()(const custom_type& a,
                                                  const custom_type& b) const noexcept {
  if (a.has_same_address(b)) {
    return true;
  }
  if (a.name() != b.name() || !all_identical(a.fields(), b.fields())) {
    return false;
  }
  if (a.underlying_type().index() != b.underlying_type().index()) {
    return false;
  }
  return overloaded_visit(
      a.underlying_type(),
      [&](const std::type_index a_index) {
        return a_index == std::get<std::type_index>(b.underlying_type());
      },
      [&](const erased_pytype& a_pytype) {
        const auto& b_pytype = std::get<erased_pytype>(b.underlying_type());
        return a_pytype.is_identical_to(b_pytype);
      });
}

field_access::field_access(custom_type type, std::string name)
    : type_(std::move(type)), field_name_(std::move(name)) {
  WF_ASSERT(!field_name_.empty(), "Field name cannot be empty");
}

struct build_access_sequence {
  // If our down-counter is on zero, this scalar is the one we want.
  // Otherwise, decrement and skip over it.
  bool operator()(const scalar_type&, std::size_t& index, std::vector<access_variant>&) const {
    if (index == 0) {
      return true;
    } else {
      --index;
      return false;
    }
  }

  // If the down-counter will reach zero within this matrix, we have found the
  // terminal access operation.
  bool operator()(const matrix_type& m, std::size_t& index,
                  std::vector<access_variant>& output) const {
    if (index < m.size()) {
      output.emplace_back(matrix_access{m, index});
      return true;
    } else {
      index -= m.size();
      return false;
    }
  }

  bool operator()(const custom_type& c, std::size_t& index,
                  std::vector<access_variant>& output) const {
    // Append every field on this type, and recurse as well into child custom types.
    for (const struct_field& field : c.fields()) {
      if (const bool found = std::visit(
              [&](const auto& child) { return operator()(child, index, output); }, field.type());
          found) {
        output.emplace_back(field_access{c, field.name()});
        return true;
      }
    }
    return false;
  }
};

// TODO: This traversal isn't necessarily blazingly efficient, since it amounts to a linear
//  search for every member access. That said, custom types are somewhat unlikely to be very deep.
std::vector<access_variant> determine_access_sequence(const custom_type& top_level_type,
                                                      std::size_t index) {
  std::vector<access_variant> sequence{};
  const bool found_member = build_access_sequence{}(top_level_type, index, sequence);
  WF_ASSERT(found_member, "Member index {} not valid for custom type `{}`", index,
            top_level_type.name());
  // This was recorded bottom to top, so flip it.
  std::reverse(sequence.begin(), sequence.end());
  return sequence;
}

numeric_primitive_type determine_member_type(const custom_type& custom, const std::size_t index) {
  const auto sequence = determine_access_sequence(custom, index);
  WF_ASSERT(!sequence.empty());

  const access_variant& last_element = sequence.back();
  return overloaded_visit(
      last_element,
      // Matrices are always float for now:
      [](const matrix_access&) constexpr { return numeric_primitive_type::floating_point; },
      [&](const field_access& access) -> numeric_primitive_type {
        // Retrieve the field from the type:
        const auto accessed_field = access.get_field();
        // Because this is a leaf, it must be a scalar. This assumption may eventually be relaxed.
        const scalar_type* const scalar = std::get_if<scalar_type>(&accessed_field->type());
        WF_ASSERT(
            scalar != nullptr,
            "Field `{}` on type `{}` is not a scalar (when accessing element {} of type `{}`)",
            access.field_name(), access.type().name(), index, custom.name());
        return scalar->numeric_type();
      });
}

}  // namespace wf
