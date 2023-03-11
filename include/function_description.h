// Copyright 2023 Gareth Cross
#pragma once
#include "ast.h"

namespace math {

struct FunctionDescription {
 public:
  template <typename... Args>
  void AddInput(Args&&... args) {
    input_args.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  template <typename... Args>
  void AddOutput(Args&&... args) {
    output_args.push_back(std::make_shared<const ast::Argument>(std::forward<Args>(args)...));
  }

  void AddReturnValue(ast::Type type) { return_values.push_back(std::move(type)); }

  std::string function_name;
  std::vector<std::shared_ptr<const ast::Argument>> input_args{};
  std::vector<std::shared_ptr<const ast::Argument>> output_args{};
  std::vector<ast::Type> return_values{};
};

}  // namespace math
