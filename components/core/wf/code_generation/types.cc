// Copyright 2023 Gareth Cross
#include "wf/code_generation/types.h"

#include <algorithm>
#include <iterator>

#include "wf/absl_imports.h"
#include "wf/assertions.h"

namespace wf {

field::field(std::string name, type_variant type) : name_(std::move(name)), type_(std::move(type)) {
  WF_ASSERT(!name_.empty(), "Field names may not be empty strings");
}

// Check that all provided field names are unique.
static void assert_field_names_are_unique(const std::vector<field>& fields) {
  if (fields.empty()) {
    return;
  }
  absl::InlinedVector<std::string_view, 8> names{};
  names.reserve(fields.size());
  std::transform(fields.begin(), fields.end(), std::back_inserter(names),
                 [](const field& f) -> std::string_view { return f.name(); });
  std::sort(names.begin(), names.end());

  for (auto it = names.begin(); std::next(it) != names.end(); ++it) {
    WF_ASSERT(*it != *std::next(it), "Custom type has duplicated field name: {}", *it);
  }
}

custom_type::custom_type(std::string name, std::vector<field> fields, std::any python_type)
    : impl_(std::make_shared<const impl>(
          impl{std::move(name), std::move(fields), std::move(python_type)})) {
  assert_field_names_are_unique(impl_->fields);
}

// TODO: Define a nullable_ptr and use it here?
const field* custom_type::field_by_name(std::string_view name) const noexcept {
  // fields_ will typically be pretty small, so just do a linear search:
  auto it = std::find_if(impl_->fields.begin(), impl_->fields.end(),
                         [&name](const field& f) { return f.name() == name; });
  if (it == impl_->fields.end()) {
    return nullptr;
  }
  return &(*it);
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
    for (const field& field : c.fields()) {
      if (const bool found = std::visit(
              [&](const auto& child) { return operator()(child, index, output); }, field.type());
          found) {
        WF_ASSERT_EQUAL(0, index);
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

}  // namespace wf
