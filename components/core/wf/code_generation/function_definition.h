// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/ast.h"
#include "wf/code_generation/function_description.h"

namespace wf {

// // Pair together a function signature with the body in ast form.
// class function_definition {
//  public:
//   // We use shared_ptr so that this object can be passed into python without copying.
//   using shared_ptr = std::shared_ptr<function_definition>;
//
//   function_definition(function_signature signature, ast::variant_vector ast)
//       : signature_(std::move(signature)), ast_(std::move(ast)) {}
//
//   // Access the signature.
//   constexpr const function_signature& signature() const noexcept { return signature_; }
//
//   // Access the abstract syntax tree.
//   constexpr const ast::variant_vector& ast() const noexcept { return ast_; }
//
//  private:
//   function_signature signature_;
//   ast::variant_vector ast_;
// };

}  // namespace wf
