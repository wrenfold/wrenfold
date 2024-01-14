#include "wf/code_generation/custom_function.h"

namespace wf {

std::shared_ptr<const custom_function::impl> custom_function::impl::create(
    std::string name, std::vector<argument> arguments, type_variant return_type) {
  impl result{std::move(name), std::move(arguments), std::move(return_type), 0};
  result.hash = hash_string_fnv(result.name);
  result.hash = hash_all(result.hash, result.arguments);
  result.hash = hash_args(result.hash, result.return_type);
  return std::make_shared<impl>(std::move(result));
}

custom_function::custom_function(std::string name, std::vector<argument> arguments,
                                 type_variant return_type)
    : impl_(impl::create(std::move(name), std::move(arguments), std::move(return_type))) {}

bool is_identical_struct<custom_function>::operator()(const custom_function& a,
                                                      const custom_function& b) const noexcept {
  if (a.has_same_address(b)) {
    return true;
  }
  return a.name() == b.name() && are_identical(a.arguments(), b.arguments()) &&
         are_identical(a.return_type(), b.return_type());
}

}  // namespace wf
