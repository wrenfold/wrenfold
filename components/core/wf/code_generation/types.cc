// Copyright 2023 Gareth Cross
#include "wf/code_generation/types.h"

#include <algorithm>
#include <iterator>

#include "wf/absl_imports.h"
#include "wf/assertions.h"

namespace wf {

custom_type::field::field(std::string name, type_variant type)
    : name_(std::move(name)), type_(std::move(type)) {
  WF_ASSERT(!name_.empty(), "Field names may not be empty strings");
  if (std::holds_alternative<custom_type::const_shared_ptr>(type_)) {
    WF_ASSERT(std::get<custom_type::const_shared_ptr>(type_),
              "Type of a field may not be null. Field name: {}", name_);
  }
}

// Check that all provided field names are unique.
static void assert_field_names_are_unique(const std::vector<custom_type::field>& fields) {
  if (fields.empty()) {
    return;
  }
  absl::InlinedVector<std::string_view, 8> names{};
  names.reserve(fields.size());
  std::transform(fields.begin(), fields.end(), std::back_inserter(names),
                 [](const auto& f) -> std::string_view { return f.name(); });
  std::sort(names.begin(), names.end());

  for (auto it = names.begin(); std::next(it) != names.end(); ++it) {
    WF_ASSERT(*it != *std::next(it), "Custom type has duplicated field name: {}", *it);
  }
}

custom_type::custom_type(std::string name, std::vector<field> fields)
    : name_(std::move(name)), fields_(std::move(fields)) {
  WF_ASSERT(!name_.empty(), "Field name cannot be empty");
  assert_field_names_are_unique(fields_);
}

// TODO: Define a nullable_ptr and use it here?
const custom_type::field* custom_type::field_by_name(std::string_view name) noexcept {
  // fields_ will typically be pretty small, so just do a linear search:
  auto it = std::find_if(fields_.begin(), fields_.end(),
                         [&name](const field& f) { return f.name() == name; });
  if (it == fields_.end()) {
    return nullptr;
  }
  return &(*it);
}

}  // namespace wf
