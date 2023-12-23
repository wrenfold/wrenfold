// Copyright 2023 Gareth Cross
#include "wf/code_generation/ast.h"

#include <regex>
#include <sstream>

namespace wf::ast {

std::vector<std::string> comment::split_lines() const {
  // Remove windows carriage return.
  const std::string content_no_cr = std::regex_replace(content, std::regex("\r\n"), "\n");
  std::stringstream stream{content_no_cr};

  std::vector<std::string> lines;
  for (std::string line; std::getline(stream, line, '\n');) {
    lines.push_back(std::move(line));
  }
  return lines;
}

std::vector<argument> function_signature::matrix_args() const {
  std::vector<argument> result{};
  result.reserve(arguments_.size());
  std::copy_if(arguments_.begin(), arguments_.end(), std::back_inserter(result),
               [](const argument& a) { return a.is_matrix(); });
  return result;
}

std::optional<argument> function_signature::argument_by_name(std::string_view str) const {
  auto it = std::find_if(arguments_.begin(), arguments_.end(),
                         [&str](const argument& arg) { return arg.name() == str; });
  if (it == arguments_.end()) {
    return std::nullopt;
  }
  return *it;
}

}  // namespace wf::ast
