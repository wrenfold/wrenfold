// Copyright 2023 Gareth Cross
#include "wf/code_generation/ast.h"

#include <regex>
#include <sstream>

namespace wf::ast {

construct_return_value::construct_return_value(argument_type type, std::vector<ast::variant>&& args)
    : type(type), args(std::move(args)) {}

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

}  // namespace wf::ast
